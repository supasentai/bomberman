{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM)
import Control.Exception (handle, catch, IOException)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (catMaybes)
import Data.List (filter)
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.Random (newStdGen, randomR, StdGen)

import Types
import GameLogic

-- Cấu hình (Giữ nguyên)
tickRate :: Int
tickRate = 30
tickDelay :: Int
tickDelay = 1000000 `div` tickRate
dt :: Float
dt = fromIntegral tickDelay / 1000000.0

-- initBoard (Giữ nguyên)
initBoard :: Board
initBoard =
  [ [ if x == 0 || y == 0 || x == 8 || y == 8 then Wall
      else if (x,y) `elem` [(1,1), (1,2), (2,1), (7,7), (7,6), (6,7)] then Empty 
      else if (x + y) `mod` 3 == 0 then Box
      else Empty
    | x <- [0..8] ]
  | y <- [0..8]
  ]

-- NÂNG CẤP: Thêm `status = Playing`
initGameState :: GameState
initGameState = GameState initBoard
  [ Player 1 (1,1) True 1 2 -- 1 bom, tầm nổ 2
  , Player 2 (7,7) True 1 2 -- 1 bom, tầm nổ 2
  ]
  [] -- bombs
  [] -- flames
  [] -- powerups
  Playing -- status

main :: IO ()
main = runServer

runServer :: IO ()
runServer = withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 2
  putStrLn "🔥 Server started at port 4242"

  stateVar   <- newTVarIO initGameState
  clientsVar <- newTVarIO []
  playerCounter <- newIORef 1
  rngVar <- newTVarIO =<< newStdGen

  _ <- forkIO $ gameLoop stateVar clientsVar rngVar

  -- Vòng lặp chấp nhận client (giữ nguyên)
  forever $ do
    (conn, _) <- accept sock
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h LineBuffering

    pid <- readIORef playerCounter
    modifyIORef playerCounter (+1)
    putStrLn $ "✅ Client connected! Assigned PlayerID: " ++ show pid

    atomically $ modifyTVar clientsVar (h:)
    gs0 <- readTVarIO stateVar
    BL.hPutStrLn h (encode gs0)

    forkIO $ clientHandler h stateVar clientsVar pid

-- NÂNG CẤP: gameLoop giờ xử lý reset game
gameLoop :: TVar GameState -> TVar [Handle] -> TVar StdGen -> IO ()
gameLoop stateVar clientsVar rngVar = forever $ do
  threadDelay tickDelay

  -- Lấy status CŨ và tick game
  (newGs, oldStatus) <- atomically $ do
    currentGs <- readTVar stateVar
    currentRng <- readTVar rngVar
    let (newGs, newRng) = tickGame dt currentRng currentGs
    writeTVar stateVar newGs
    writeTVar rngVar newRng
    return (newGs, status currentGs) -- Trả về state MỚI và status CŨ
  
  -- Gửi state mới cho client
  handles <- readTVarIO clientsVar
  newHandles <- broadcast handles newGs
  atomically $ writeTVar clientsVar newHandles
  
  -- KIỂM TRA RESET GAME (MỚI)
  -- Chỉ reset khi game vừa chuyển từ Playing sang GameOver
  case (oldStatus, status newGs) of
    (Playing, GameOver _) -> do
      putStrLn "Game over! Resetting in 5 seconds..."
      threadDelay 5000000 -- Chờ 5 giây
      atomically $ writeTVar stateVar initGameState -- Reset
      putStrLn "Game reset!"
    (Playing, Draw) -> do
      putStrLn "Draw! Resetting in 5 seconds..."
      threadDelay 5000000 -- Chờ 5 giây
      atomically $ writeTVar stateVar initGameState -- Reset
      putStrLn "Game reset!"
    _ -> return () -- Các trường hợp khác: không làm gì

-- broadcast (Giữ nguyên)
broadcast :: [Handle] -> GameState -> IO [Handle]
broadcast handles gs = do
  results <- forM handles $ \h -> do
    catch (do
             BL.hPutStrLn h (encode gs)
             return (Just h)
           )
          (\e -> let _ = e :: IOException in return Nothing)
  return (catMaybes results)

-- clientHandler (Giữ nguyên)
clientHandler :: Handle -> TVar GameState -> TVar [Handle] -> Int -> IO ()
clientHandler h stateVar clientsVar pid =
  handle (disconnectHandler h clientsVar) $
    forever $ do
      line <- hGetLine h
      newGs <- atomically $ do
          gs <- readTVar stateVar
          let gs' = updateFromCommand gs line pid
          writeTVar stateVar gs'
          return gs'
      catch (BL.hPutStrLn h (encode newGs))
            (\e -> let _ = e :: IOException in return ())

-- disconnectHandler (Giữ nguyên)
disconnectHandler :: Handle -> TVar [Handle] -> IOException -> IO ()
disconnectHandler h clientsVar _ = do
  putStrLn "Client disconnected."
  atomically $ modifyTVar clientsVar (filter (/= h))

-- updateFromCommand (Giữ nguyên)
updateFromCommand :: GameState -> String -> Int -> GameState
updateFromCommand gs cmd pid
    | cmd == "w" = movePlayer pid ( 0, -1) gs
    | cmd == "s" = movePlayer pid ( 0,  1) gs
    | cmd == "a" = movePlayer pid (-1,  0) gs
    | cmd == "d" = movePlayer pid ( 1,  0) gs
    | cmd == "b" = dropBomb pid gs
    | otherwise  = gs
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
import Data.IORef (newIORef, readIORef, modifyIORef) -- Thêm import

import Types
import GameLogic

-- Cấu hình tick rate
tickRate :: Int
tickRate = 30

tickDelay :: Int
tickDelay = 1000000 `div` tickRate

dt :: Float
dt = fromIntegral tickDelay / 1000000.0

-- initBoard (Đã sửa lỗi kẹt player)
initBoard :: Board
initBoard =
  [ [ if x == 0 || y == 0 || x == 8 || y == 8 then Wall
      else if (x,y) `elem` [(1,1), (1,2), (2,1), (7,7), (7,6), (6,7)] then Empty 
      else if (x + y) `mod` 3 == 0 then Box
      else Empty
    | x <- [0..8] ]
  | y <- [0..8]
  ]

-- initGameState (Giữ nguyên)
initGameState :: GameState
initGameState = GameState initBoard
  [ Player 1 (1,1) True
  , Player 2 (7,7) True
  ]
  [] []


-- | Luồng chính của server
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
  
  -- TẠO MỚI: Biến đếm player ID
  playerCounter <- newIORef 1 

  _ <- forkIO $ gameLoop stateVar clientsVar

  -- Vòng lặp chấp nhận client mới
  forever $ do
    (conn, _) <- accept sock
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h LineBuffering

    -- LẤY ID CHO PLAYER MỚI:
    pid <- readIORef playerCounter
    modifyIORef playerCounter (+1) -- Tăng biến đếm cho người tiếp theo
    
    putStrLn $ "✅ Client connected! Assigned PlayerID: " ++ show pid

    atomically $ modifyTVar clientsVar (h:)
    gs0 <- readTVarIO stateVar
    BL.hPutStrLn h (encode gs0)

    -- SỬA ĐỔI: Truyền `pid` vào clientHandler
    forkIO $ clientHandler h stateVar clientsVar pid

-- | Vòng lặp game chính (chạy độc lập)
gameLoop :: TVar GameState -> TVar [Handle] -> IO ()
gameLoop stateVar clientsVar = forever $ do
  threadDelay tickDelay

  gs <- atomically $ do
    currentGs <- readTVar stateVar
    let newGs = tickGame dt currentGs
    writeTVar stateVar newGs
    return newGs

  handles <- readTVarIO clientsVar
  newHandles <- broadcast handles gs
  atomically $ writeTVar clientsVar newHandles

-- | Gửi state cho tất cả client, trả về danh sách client còn sống
broadcast :: [Handle] -> GameState -> IO [Handle]
broadcast handles gs = do
  results <- forM handles $ \h -> do
    catch (do
             BL.hPutStrLn h (encode gs)
             return (Just h)
           )
          (\e -> let _ = e :: IOException in return Nothing)
  return (catMaybes results)

-- | Xử lý input từ một client
-- SỬA ĐỔI: Thêm tham số `pid` (PlayerID)
clientHandler :: Handle -> TVar GameState -> TVar [Handle] -> Int -> IO ()
clientHandler h stateVar clientsVar pid =
  handle (disconnectHandler h clientsVar) $
    forever $ do
      line <- hGetLine h
      atomically $ do
          gs <- readTVar stateVar
          -- SỬA ĐỔI: Truyền `pid` vào updateFromCommand
          let gs' = updateFromCommand gs line pid
          writeTVar stateVar gs'

-- | Xử lý khi client ngắt kết nối
disconnectHandler :: Handle -> TVar [Handle] -> IOException -> IO ()
disconnectHandler h clientsVar _ = do
  putStrLn "Client disconnected."
  atomically $ modifyTVar clientsVar (filter (/= h))

-- | Cập nhật game dựa trên lệnh từ client
-- SỬA ĐỔI: Thêm tham số `pid` và dùng nó
updateFromCommand :: GameState -> String -> Int -> GameState
updateFromCommand gs cmd pid
    | cmd == "w" = movePlayer pid ( 0, -1) gs
    | cmd == "s" = movePlayer pid ( 0,  1) gs
    | cmd == "a" = movePlayer pid (-1,  0) gs
    | cmd == "d" = movePlayer pid ( 1,  0) gs
    | cmd == "b" = dropBomb pid gs
    | otherwise  = gs

main :: IO ()
main = runServer
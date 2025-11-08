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
import Data.List (filter, isPrefixOf, foldl')
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.Random (newStdGen, randomR, StdGen)
import qualified Data.Set as Set
import Data.Set (Set)

-- Import thư viện
import Types
import GameLogic

-- Cấu hình (Giữ nguyên)
tickRate :: Int
tickRate = 30
tickDelay :: Int
tickDelay = 1000000 `div` tickRate
dt :: Float
dt = fromIntegral tickDelay / 1000000.0

-- ========== LOGIC TẠO MÊ CUNG (Giữ nguyên) ==========
mazeWidth :: Int
mazeHeight :: Int
mazeWidth = 21
mazeHeight = 21

getNeighbors :: (Int, Int) -> Set (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) visited =
  let potential = [(x+2, y), (x-2, y), (x, y+2), (x, y-2)]
  in filter (\(nx, ny) -> nx > 0 && nx < mazeWidth - 1 &&
                        ny > 0 && ny < mazeHeight - 1 &&
                        not (Set.member (nx, ny) visited)) potential

wallBetween :: (Int, Int) -> (Int, Int) -> (Int, Int)
wallBetween (x1, y1) (x2, y2) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

updateBoardCell :: Board -> (Int, Int) -> Cell -> Board
updateBoardCell b (x, y) cell =
  take y b ++
  [take x (b !! y) ++ [cell] ++ drop (x + 1) (b !! y)] ++
  drop (y + 1) b

carveMaze :: StdGen -> Set (Int, Int) -> [(Int, Int)] -> Board -> (Board, StdGen)
carveMaze rng visited [] board = (board, rng)
carveMaze rng visited stack@(current:restStack) board =
  let (neighbors, newRng) = case getNeighbors current visited of
                            [] -> ([], rng)
                            ns -> let (idx, r) = randomR (0, length ns - 1) rng
                                  in ([ns !! idx], r)
  in
    case neighbors of
      [] ->
        carveMaze newRng visited restStack board
      (next:_) ->
        let 
          wallPos = wallBetween current next
          board' = updateBoardCell board wallPos Empty
          board'' = updateBoardCell board' next Empty
          visited' = Set.insert next visited
          stack' = next : stack
        in
          carveMaze newRng visited' stack' board''

generateMaze :: StdGen -> (Board, StdGen)
generateMaze rng =
  let 
    initialBoard = replicate mazeHeight (replicate mazeWidth Wall)
    startPos = (1, 1)
    boardWithStart = updateBoardCell initialBoard startPos Empty 
    visited = Set.singleton startPos
    stack = [startPos]
    (mazeBoard, r1) = carveMaze rng visited stack boardWithStart
  in 
    (mazeBoard, r1)

sprinkleBoxes :: StdGen -> Board -> (Board, StdGen)
sprinkleBoxes rng board =
  let
      emptyCells = [(x, y) | y <- [1..mazeHeight-2], x <- [1..mazeWidth-2], 
                             (board !! y) !! x == Empty]
      boxChance = 0.4 :: Float
      (newBoard, newRng) = foldl' (sprinkleOne boxChance) (board, rng) emptyCells
  in (newBoard, newRng)

sprinkleOne :: Float -> (Board, StdGen) -> (Int, Int) -> (Board, StdGen)
sprinkleOne chance (b, r) pos =
  let (roll, r') = randomR (0.0, 1.0) r
  in if roll < chance
     then (updateBoardCell b pos Box, r')
     else (b, r')

createSafeZone :: Board -> Board
createSafeZone board =
  let 
    p1Zone = [(1,1), (1,2), (2,1)]
    p2Pos = (mazeWidth-2, mazeHeight-2)
    p2Zone = [p2Pos, (fst p2Pos - 1, snd p2Pos), (fst p2Pos, snd p2Pos - 1)]
    
    clearedBoard = foldl' (\b pos -> updateBoardCell b pos Empty) board (p1Zone ++ p2Zone)
  in
    clearedBoard

findRandomEmpty :: StdGen -> Board -> Int -> ([(Int, Int)], StdGen)
findRandomEmpty rng _ 0 = ([], rng)
findRandomEmpty rng board n =
  let 
      emptyCells = [(x, y) | y <- [1..mazeHeight-2], x <- [1..mazeWidth-2], 
                             (board !! y) !! x == Empty,
                             (x,y) /= (1,1), (x,y) /= (mazeWidth-2, mazeHeight-2)]
      (idx, r') = randomR (0, length emptyCells - 1) rng
      pos = emptyCells !! idx
      (remainingPos, r'') = findRandomEmpty r' board (n-1)
  in (pos : remainingPos, r'')

-- NÂNG CẤP: `initialGameState` (Thêm type, timer)
initialGameState :: StdGen -> (GameState, StdGen)
initialGameState rng =
  let (mazeBoard, r1) = generateMaze rng
      (boxedBoard, r2) = sprinkleBoxes r1 mazeBoard
      finalBoard = createSafeZone boxedBoard 
      
      (monsterPos, r3) = findRandomEmpty r2 finalBoard 5 
      -- MỚI: Khởi tạo tất cả là 'Grunt'
      monsters = [Monster i pos Grunt | (i, pos) <- zip [1..] monsterPos]
      
      players = [ Player 1 (1,1) True 1 1
                , Player 2 (mazeWidth-2, mazeHeight-2) True 1 1 ]
  in
    (GameState
      { board = finalBoard
      , players = players
      , bombs = []
      , flames = []
      , powerups = []
      , status = Playing
      , chatHistory = []
      , monsters = monsters
      , monsterMoveTimer = 0.0
      , gamePhaseTimer = 30.0 -- MỚI: Khởi tạo 30 giây
      }, r3)
-- ========== KẾT THÚC LOGIC MÊ CUNG ==========

main :: IO ()
main = runServer

-- runServer (Giữ nguyên)
runServer :: IO ()
runServer = withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  case addrinfos of
    [] -> ioError (userError "runServer: getAddrInfo returned an empty list")
    (serveraddr:_) -> do
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      bind sock (addrAddress serveraddr)
      listen sock 2
      putStrLn "🔥 Server started at port 4242"
      
      initRng <- newStdGen
      let (gs0, rng1) = initialGameState initRng
      stateVar   <- newTVarIO gs0
      clientsVar <- newTVarIO []
      playerCounter <- newIORef 1
      rngVar     <- newTVarIO rng1

      _ <- forkIO $ gameLoop stateVar clientsVar rngVar

      forever $ do
        (conn, _) <- accept sock
        h <- socketToHandle conn ReadWriteMode
        hSetBuffering h LineBuffering

        pid <- readIORef playerCounter
        modifyIORef playerCounter (+1)
        putStrLn $ "✅ Client connected! Assigned PlayerID: " ++ show pid

        atomically $ modifyTVar clientsVar (h:)
        gs0_current <- readTVarIO stateVar
        BL.hPutStrLn h (encode gs0_current)

        forkIO $ clientHandler h stateVar clientsVar pid

-- gameLoop (Giữ nguyên)
gameLoop :: TVar GameState -> TVar [Handle] -> TVar StdGen -> IO ()
gameLoop stateVar clientsVar rngVar = forever $ do
  threadDelay tickDelay
  
  (newGs, oldStatus) <- atomically $ do
    currentGs <- readTVar stateVar
    currentRng <- readTVar rngVar
    let (newGs, newRng) = tickGame dt currentRng currentGs
    writeTVar stateVar newGs
    writeTVar rngVar newRng
    return (newGs, status currentGs)
  
  handles <- readTVarIO clientsVar
  newHandles <- broadcast handles newGs
  atomically $ writeTVar clientsVar newHandles
  
  case (oldStatus, status newGs) of
    (Playing, GameOver _) -> resetGame "Game over"
    (Playing, Draw)       -> resetGame "Draw"
    _ -> return ()
  where
    resetGame msg = do
      putStrLn $ msg ++ "! Resetting in 5 seconds..."
      threadDelay 5000000
      newRng <- atomically $ do
                  r <- readTVar rngVar
                  let (newGs, r') = initialGameState r
                  writeTVar stateVar newGs
                  writeTVar rngVar r'
                  return r'
      putStrLn "Game reset! New maze generated."

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
    | "/say " `isPrefixOf` cmd =
        let msg = drop 5 cmd
            formattedMsg = "P" ++ show pid ++ ": " ++ msg
        in 
           gs { chatHistory = take 10 (formattedMsg : chatHistory gs) }
    | cmd == "w" = movePlayer pid ( 0, -1) gs
    | cmd == "s" = movePlayer pid ( 0,  1) gs
    | cmd == "a" = movePlayer pid (-1,  0) gs
    | cmd == "d" = movePlayer pid ( 1,  0) gs
    | cmd == "b" = dropBomb pid gs
    | otherwise  = gs
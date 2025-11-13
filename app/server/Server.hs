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

import Types
import GameLogic

tickRate :: Int
tickRate = 30
tickDelay :: Int
tickDelay = 1000000 `div` tickRate
dt :: Float
dt = fromIntegral tickDelay / 1000000.0

-- ========== LOGIC TẠO MÊ CUNG ==========
mazeWidth :: Int
mazeHeight :: Int
mazeWidth = 15
mazeHeight = 15

generateExactMaze :: StdGen -> (Board, StdGen)
generateExactMaze rng = (exactBoard, rng)

exactBoard :: Board
exactBoard = [
  [Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall],
  [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Empty, Empty, Wall],
  [Wall, Empty, Wall,  Wall,  Empty, Wall,  Wall,  Empty, Wall,  Empty, Empty, Wall,  Wall,  Empty, Wall],
  [Wall, Empty, Wall,  Empty,  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall,  Empty, Wall],
  [Wall, Empty, Empty, Empty,  Empty, Empty, Wall,  Wall,  Wall,  Empty, Wall,  Empty, Empty, Empty, Wall],
  [Wall, Wall,  Empty, Wall,  Empty, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Wall,  Wall,  Wall],
  [Wall, Empty, Empty, Empty, Wall,  Wall,  Empty, Empty, Empty, Empty, Wall,  Empty, Empty, Empty, Wall],
  [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall,  Wall,  Empty, Wall,  Wall],
  [Wall, Empty, Wall,  Wall,  Empty, Wall,  Wall,  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall],
  [Wall, Empty, Empty, Wall,  Empty, Empty, Empty, Empty, Wall,  Wall,  Wall,  Empty, Wall,  Empty, Wall],
  [Wall, Wall,  Empty, Wall,  Empty, Wall,  Wall,  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall],
  [Wall, Empty, Empty, Empty, Empty, Empty, Wall,  Wall,  Wall,  Empty, Wall,  Empty, Wall,  Empty, Wall],
  [Wall, Empty, Empty, Wall,  Wall,  Empty, Empty, Empty, Empty, Empty, Wall,  Empty, Wall,  Empty,  Wall],
  [Wall, Empty, Empty, Empty, Wall,  Wall,  Empty, Empty, Wall,  Empty, Empty, Wall,  Empty, Empty, Wall],
  [Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall]
  ]

-- generateMaze: Always return the exact board
generateMaze :: StdGen -> (Board, StdGen)
generateMaze rng = generateExactMaze rng

sprinkleBoxes :: StdGen -> Board -> (Board, StdGen)
sprinkleBoxes rng board =
  let numBoxes = 75
      (positions, rng') = findRandomEmpty rng board numBoxes False
      newBoard = foldl' (\b pos -> updateBoardCell b pos Box) board positions
  in (newBoard, rng')

-- createSafeZone: Clear spawn areas
createSafeZone :: Board -> Board
createSafeZone board =
  let 
    p1RedZone = [(1,1), (1,2), (2,1), (3,1), (1,3)]
    p2RedZone = [(13,13), (11,13), (12,13), (13,12), (13,11)]
    safeRedCells = p1RedZone ++ p2RedZone  
    clearedBoard = foldl' (\b pos -> updateBoardCell b pos Empty) board safeRedCells
  in
    clearedBoard

-- findRandomEmpty: Avoid spawn zones
findRandomEmpty :: StdGen -> Board -> Int -> Bool -> ([(Int, Int)], StdGen)
findRandomEmpty rng _ 0 _ = ([], rng)
findRandomEmpty rng board n isForMonsters =
  let 
    redForbidden = [(1,1),(2,1),(3,1),(1,2),(2,2),(3,2),(4,1),(5,1),(4,2),(1,3),(1,4),(2,4),(3,3),(3,4),
                                           (11,13),(12,13),(13,13),(11,12),(12,12),(13,12),(13,11),(13,10),(12,10),(11,10),(11,11),(10,13),(9,13),(9,12),(13,9),(13,8),(9,12),(9,11),(12,8),(11,8)]
    candidates = [(x,y) | y <- [0..14], x <- [0..14],
                          (board !! y) !! x == Empty,
                          not (isForMonsters && elem (x,y) redForbidden)]
    
    len = length candidates
  in
    if len == 0 then ([], rng) else
      let (idx, r') = randomR (0, len - 1) rng
          pos = candidates !! idx
          (rest, r'') = findRandomEmpty r' board (n-1) isForMonsters
      in (pos : rest, r'')

updateBoardCell :: Board -> (Int, Int) -> Cell -> Board
updateBoardCell b (x, y) cell =
  take y b ++
  [take x (b !! y) ++ [cell] ++ drop (x + 1) (b !! y)] ++
  drop (y + 1) b

createCoopGame :: StdGen -> (GameState, StdGen)
createCoopGame rng =
  let (mazeBoard, r1) = generateMaze rng
      (boxedBoard, r2) = sprinkleBoxes r1 mazeBoard
      finalBoard = createSafeZone boxedBoard 
      
      (monsterPos, r3) = findRandomEmpty r2 finalBoard 3 True
      monsters = [Monster i pos Grunt | (i, pos) <- zip [1..] monsterPos]
      
      players = [ Player 1 (1,1) True 1 1 False 0.0 0.0 False 
                , Player 2 (mazeWidth-2, mazeHeight-2) True 1 1 False 0.0 0.0 False
                ]
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
      , gamePhaseTimer = 30.0
      , playerAIMoveTimer = 0.0
      }, r3)

create1v1Game :: StdGen -> (GameState, StdGen)
create1v1Game rng =
  let (mazeBoard, r1) = generateMaze rng
      (boxedBoard, r2) = sprinkleBoxes r1 mazeBoard
      finalBoard = createSafeZone boxedBoard 
      
      (monsterPos, r3) = findRandomEmpty r2 finalBoard 2 True
      monsters = [Monster i pos Grunt | (i, pos) <- zip [1..] monsterPos]
      
      players = [ Player 1 (1,1) True 1 1 False 0.0 0.0 False 
                , Player 2 (mazeWidth-2, mazeHeight-2) True 1 1 False 0.0 0.0 True
                ]
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
      , gamePhaseTimer = 30.0
      , playerAIMoveTimer = 0.0
      }, r3)

lobbyGameState :: GameState
lobbyGameState = GameState
  { board = [[]]
  , players = []
  , bombs = []
  , flames = []
  , powerups = []
  , status = Lobby
  , chatHistory = []
  , monsters = []
  , monsterMoveTimer = 0.0
  , gamePhaseTimer = 0.0
  , playerAIMoveTimer = 0.0
  }

-- ========== KẾT THÚC LOGIC MÊ CUNG ==========

main :: IO ()
main = runServer

-- runServer
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
      
      stateVar   <- newTVarIO lobbyGameState
      clientsVar <- newTVarIO []
      playerCounter <- newIORef 1
      rngVar     <- newTVarIO =<< newStdGen

      _ <- forkIO $ gameLoop stateVar clientsVar rngVar

      forever $ do
        (conn, _) <- accept sock
        h <- socketToHandle conn ReadWriteMode
        hSetBuffering h LineBuffering

        pid <- readIORef playerCounter
        modifyIORef playerCounter (+1)
        putStrLn $ "✅ Client connected! Assigned PlayerID: " ++ show pid

        hPutStrLn h ("PID:" ++ show pid)
        hFlush h

        atomically $ modifyTVar clientsVar (h:)
        gs0_current <- readTVarIO stateVar
        BL.hPutStrLn h (encode gs0_current)

        forkIO $ clientHandler h stateVar clientsVar rngVar pid

--gameLoop
gameLoop :: TVar GameState -> TVar [Handle] -> TVar StdGen -> IO ()
gameLoop stateVar clientsVar rngVar = forever $ do
  threadDelay tickDelay
  
  (newGs, oldStatus) <- atomically $ do
    currentGs <- readTVar stateVar
    currentRng <- readTVar rngVar
    let (newGs, newRng) = if status currentGs == Playing
                          then tickGame dt currentRng currentGs
                          else (currentGs, currentRng)
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
      putStrLn $ msg ++ "! Resetting to Lobby in 5 seconds..."
      threadDelay 5000000
      atomically $ writeTVar stateVar lobbyGameState
      putStrLn "Game reset! Returned to Lobby."

-- broadcast
broadcast :: [Handle] -> GameState -> IO [Handle]
broadcast handles gs = do
  results <- forM handles $ \h -> do
    catch (do
             BL.hPutStrLn h (encode gs)
             return (Just h)
           )
          (\e -> let _ = e :: IOException in return Nothing)
  return (catMaybes results)

--clientHandler
clientHandler :: Handle -> TVar GameState -> TVar [Handle] -> TVar StdGen -> Int -> IO ()
clientHandler h stateVar clientsVar rngVar pid =
  handle (disconnectHandler h clientsVar) $
    forever $ do
      line <- hGetLine h
      if "/mode " `isPrefixOf` line
      then do
        let mode = drop 6 line
        (newGs, newRng) <- atomically $ do
          r <- readTVar rngVar
          let (newGs, r') = if mode == "coop"
                            then createCoopGame r
                            else create1v1Game r
          writeTVar stateVar newGs
          writeTVar rngVar r'
          return (newGs, r')
          
        putStrLn $ "Player " ++ show pid ++ " started game mode: " ++ mode
        handles <- readTVarIO clientsVar
        newHandles <- broadcast handles newGs
        atomically $ writeTVar clientsVar newHandles
        
      else do
        newGs <- atomically $ do
            gs <- readTVar stateVar
            let gs' = if status gs == Playing
                      then updateFromCommand gs line pid
                      else gs
            writeTVar stateVar gs'
            return gs'
        catch (BL.hPutStrLn h (encode newGs))
              (\e -> let _ = e :: IOException in return ())

-- disconnectHandler
disconnectHandler :: Handle -> TVar [Handle] -> IOException -> IO ()
disconnectHandler h clientsVar _ = do
  putStrLn "Client disconnected."
  atomically $ modifyTVar clientsVar (filter (/= h))

-- updateFromCommand
updateFromCommand :: GameState -> String -> Int -> GameState
updateFromCommand gs cmd pid
    | "/say " `isPrefixOf` cmd =
        let msg = drop 5 cmd
            formattedMsg = "P" ++ show pid ++ ": " ++ msg
        in 
           gs { chatHistory = take 10 (formattedMsg : chatHistory gs) }
    
    | "/spawn " `isPrefixOf` cmd =
        let itemTypeStr = drop 7 cmd
        in spawnItemNearPlayer pid itemTypeStr gs

    | cmd == "w" = movePlayer pid ( 0, -1) gs
    | cmd == "s" = movePlayer pid ( 0,  1) gs
    | cmd == "a" = movePlayer pid (-1,  0) gs
    | cmd == "d" = movePlayer pid ( 1,  0) gs
    | cmd == "b" = dropBomb pid gs
    | otherwise  = gs
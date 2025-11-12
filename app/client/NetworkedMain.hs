{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import Network.Socket
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.IORef
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isPrint)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (isPrefixOf)

-- Import các module từ thư viện (trong 'src/')
import Types
import Render

-- Tốc độ di chuyển (ô/giây)
animationSpeed :: Float
animationSpeed = 10.0

-- ClientState (Giữ nguyên)
data ClientState = ClientState
  { connHandle     :: Handle
  , gameVar        :: IORef GameState -- Trạng thái logic (từ server)
  , visualPlayers  :: IORef (Map Int (Float, Float)) -- Vị trí hình ảnh
  , isTyping       :: IORef Bool
  , chatBuffer     :: IORef String
  , myPlayerId     :: IORef Int
  }

-- connectServer (Giữ nguyên)
connectServer :: String -> String -> IO Handle
connectServer host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  case addrinfos of
    [] -> ioError (userError "connectServer: getAddrInfo returned an empty list")
    (serveraddr:_) -> do
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      h <- socketToHandle sock ReadWriteMode
      hSetBuffering h LineBuffering
      putStrLn " Connected to Bomberman server!"
      return h

-- recvLoop (Giữ nguyên)
recvLoop :: ClientState -> IO ()
recvLoop st@ClientState{..} = forever $ do
  msgLine <- hGetLine connHandle
  let msg = BL.pack msgLine
  case decode msg of
    Just gs -> writeIORef gameVar gs
    Nothing -> putStrLn " Parse error from server"

-- main (SỬA LỖI + THÊM XỬ LÝ PID)
main :: IO ()
main = do
  h <- connectServer "127.0.0.1" "4242"
  
  firstLine <- hGetLine h
  pid <- if "PID:" `isPrefixOf` firstLine
    then do
      let pidStr = drop 4 firstLine
      putStrLn $ "You are P" ++ pidStr ++ "!"
      return (read pidStr :: Int)
    else do
      putStrLn $ "Error: Expected PID from server, got: " ++ firstLine
      return 0  -- fallback

  -- Lưu PID vào IORef
  myPidRef <- newIORef pid

  initGame <- newIORef (GameState [[]] [] [] [] [] Lobby [] [] 0.0 0.0 0.0)
  typingRef <- newIORef False
  bufferRef <- newIORef ""
  visualsRef <- newIORef Map.empty

  let st = ClientState h initGame visualsRef typingRef bufferRef myPidRef
  
  _ <- forkIO (recvLoop st)
  playIO
    (InWindow "Bomberman Client" (800, 600) (100, 100))
    black
    30
    st
    drawState
    handleInput
    updateFunc

-- HÀM MỚI (Giữ nguyên)
moveTowards :: Float -> Float -> Float -> Float
moveTowards current target amount
  | current < target = min (current + amount) target
  | current > target = max (current - amount) target
  | otherwise        = target

-- HÀM MỚI (Giữ nguyên)
updateFunc :: Float -> ClientState -> IO ClientState
updateFunc dt st@ClientState{..} = do
  targetGs <- readIORef gameVar
  currentVisuals <- readIORef visualPlayers
  
  case status targetGs of
    Playing -> do
      let 
        newVisuals = foldl (updateVisualPlayer dt) currentVisuals (players targetGs)
      writeIORef visualPlayers newVisuals
    _ -> 
      writeIORef visualPlayers Map.empty
      
  return st

-- HÀM MỚI (Giữ nguyên)
updateVisualPlayer :: Float -> Map Int (Float, Float) -> Player -> Map Int (Float, Float)
updateVisualPlayer dt visuals p =
  let 
    pid = playerId p
    (targetX, targetY) = (fromIntegral $ fst (pos p), fromIntegral $ snd (pos p))
    (currentX, currentY) = Map.findWithDefault (targetX, targetY) pid visuals
    moveAmount = animationSpeed * dt
    newX = moveTowards currentX targetX moveAmount
    newY = moveTowards currentY targetY moveAmount
  in
    Map.insert pid (newX, newY) visuals

-- NÂNG CẤP: `drawState` (Giữ nguyên)
drawState :: ClientState -> IO Picture
drawState ClientState{..} = do
  gs <- readIORef gameVar
  typing <- readIORef isTyping
  buffer <- readIORef chatBuffer
  visuals <- readIORef visualPlayers
  myPid <- readIORef myPlayerId  -- LẤY PID

  let gamePic = drawGame gs visuals 

  let chatUI = if status gs /= Lobby
               then Pictures [ drawChatHistory (chatHistory gs)
                             , drawChatInput typing buffer ]
               else Blank

  -- THÊM: Hiển thị "You are P1" / "You are P2"
  let playerLabel = if status gs == Playing && myPid > 0
        then Translate (-380) 280 $
             Scale 0.15 0.15 $
             Color green $
             Text ("You are P" ++ show myPid)
        else Blank

  return (Pictures [gamePic, chatUI, playerLabel])

-- NÂNG CẤP: `handleInput` (Giữ nguyên)
handleInput :: Event -> ClientState -> IO ClientState
handleInput event st@ClientState{..} = do
  typing <- readIORef isTyping
  gs <- readIORef gameVar
  
  case status gs of
    Lobby -> handleLobby event st
    _     -> if typing
             then handleTyping event st
             else handlePlaying event st

-- HÀM MỚI: Xử lý input khi ở Sảnh chờ (Lobby)
handleLobby :: Event -> ClientState -> IO ClientState
handleLobby (EventKey (Char '1') Down _ _) st@ClientState{..} = do
  putStrLn "CLIENT: Requesting Co-op mode"
  hPutStrLn connHandle "/mode coop"
  hFlush connHandle
  return st
handleLobby (EventKey (Char '2') Down _ _) st@ClientState{..} = do
  putStrLn "CLIENT: Requesting 1v1 AI mode"
  hPutStrLn connHandle "/mode 1v1"
  hFlush connHandle
  return st
handleLobby _ st = return st

-- NÂNG CẤP: `handlePlaying` (Giữ nguyên)
handlePlaying :: Event -> ClientState -> IO ClientState
handlePlaying (EventKey (SpecialKey KeyEnter) Down _ _) st@ClientState{..} = do
  writeIORef isTyping True
  return st
handlePlaying (EventKey (Char c) Down _ _) st@ClientState{..}
  | c `elem` ("wasd" :: String) = do
      hPutStrLn connHandle [c]
      hFlush connHandle
      return st
  | c == 'b' = do
      hPutStrLn connHandle "b"
      hFlush connHandle
      return st
  | c == '1' = do
      hPutStrLn connHandle "/spawn bombup"
      hFlush connHandle
      return st
  | c == '2' = do
      hPutStrLn connHandle "/spawn flameup"
      hFlush connHandle
      return st
  | c == '3' = do
      hPutStrLn connHandle "/spawn shield"
      hFlush connHandle
      return st
  | c == '4' = do
      hPutStrLn connHandle "/spawn chaos"
      hFlush connHandle
      return st
handlePlaying _ st = return st

-- handleTyping (Giữ nguyên)
handleTyping :: Event -> ClientState -> IO ClientState
handleTyping (EventKey (SpecialKey KeyEnter) Down _ _) st@ClientState{..} = do
  buffer <- readIORef chatBuffer
  if not (null buffer)
  then do
    hPutStrLn connHandle ("/say " ++ buffer)
    hFlush connHandle
    writeIORef chatBuffer ""
  else
    return ()
  
  writeIORef isTyping False
  return st
handleTyping (EventKey (SpecialKey KeyBackspace) Down _ _) st@ClientState{..} = do
  modifyIORef chatBuffer (\b -> if null b then "" else init b)
  return st
handleTyping (EventKey (Char '\b') Down _ _) st@ClientState{..} = do
  modifyIORef chatBuffer (\b -> if null b then "" else init b)
  return st
handleTyping (EventKey (Char c) Down _ _) st@ClientState{..}
  | isPrint c = do
      modifyIORef chatBuffer (\b -> b ++ [c])
      return st
handleTyping _ st = return st
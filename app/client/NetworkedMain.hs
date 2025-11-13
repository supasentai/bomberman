{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss.Interface.IO.Game hiding (loadBMP)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss (Picture(Pictures), Color, color, black, white, red, blue, green, yellow, orange, magenta, greyN, rectangleSolid, circleSolid, translate)
import Control.Exception (catch, IOException)

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

import Types
import Render

-- Tốc độ di chuyển (ô/giây)
animationSpeed :: Float
animationSpeed = 10.0

-- ClientState
data ClientState = ClientState
  { connHandle     :: Handle
  , gameVar        :: IORef GameState
  , visualPlayers  :: IORef (Map Int (Float, Float))
  , isTyping       :: IORef Bool
  , chatBuffer     :: IORef String
  , myPlayerId     :: IORef Int
  , assets         :: GameAssets
  }

-- connectServer
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

-- recvLoop
recvLoop :: ClientState -> IO ()
recvLoop st@ClientState{..} = forever $ do
  msgLine <- hGetLine connHandle
  let msg = BL.pack msgLine
  case decode msg of
    Just gs -> writeIORef gameVar gs
    Nothing -> putStrLn " Parse error from server"

--nếu Tải BMP lỗi thì dùng fallback
loadOrMake :: FilePath -> Picture -> IO Picture
loadOrMake filepath fallback = 
  (loadBMP filepath) `catch` handleErr
  where
    handleErr :: IOException -> IO Picture
    handleErr e = do
      putStrLn $ "Warning: Could not load '" ++ filepath ++ "'. Using fallback picture."
      return fallback

--Tải tất cả ảnh
loadAssets :: IO GameAssets
loadAssets = do
  putStrLn "Loading assets (with fallbacks)..."
  
  let size = 40 -- Kích thước của fallback
      assetPath = "assets/" 
      fp f = assetPath ++ f

  pWall    <- loadOrMake (fp "wall.bmp")    (Pictures [color (greyN 0.2) (rectangleSolid size size), color (greyN 0.1) (rectangleSolid (size*0.9) (size*0.9))])
  pBox     <- loadOrMake (fp "box.bmp")     (color (greyN 0.4) (rectangleSolid size size))
  pEmpty   <- loadOrMake (fp "empty.bmp")   (color (greyN 0.7) (rectangleSolid size size))
  pPlayer1 <- loadOrMake (fp "p1.bmp")    (color red (circleSolid (size/3)))
  pPlayer2 <- loadOrMake (fp "p2.bmp")    (color blue (circleSolid (size/3)))
  pBomb    <- loadOrMake (fp "bomb.bmp")    (Pictures [color black (circleSolid (size/2.5)), color white (translate 0 (size/3) (rectangleSolid (size/10) (size/4)))])
  pFlame   <- loadOrMake (fp "flame.bmp")   (color orange (rectangleSolid (size*0.9) (size*0.9)))
  pMonster <- loadOrMake (fp "monster.bmp") (color green (rectangleSolid (size*0.7) (size*0.7)))
  pGhost   <- loadOrMake (fp "ghost.bmp")   (color (makeColor 200 100 255 255) (rectangleSolid (size*0.7) (size*0.7)))
  pBombUp  <- loadOrMake (fp "bombup.bmp")  (color yellow (circleSolid (size/4)))
  pFlameUp <- loadOrMake (fp "flameup.bmp") (color orange (circleSolid (size/4)))
  pShield  <- loadOrMake (fp "shield.bmp")  (color white (circleSolid (size/4)))
  pChaos   <- loadOrMake (fp "chaos.bmp")   (color magenta (circleSolid (size/4)))
  pBrokenShield <- loadOrMake (fp "broken_shield.bmp") (color (greyN 0.5) (circleSolid (size * 0.5)))
  
  putStrLn "Assets loading complete."
  return GameAssets{..}

main :: IO ()
main = do
  gameAssets <- loadAssets
  h <- connectServer "127.0.0.1" "4242"
  
  firstLine <- hGetLine h
  pid <- if "PID:" `isPrefixOf` firstLine
    then do
      let pidStr = drop 4 firstLine
      putStrLn $ "You are P" ++ pidStr ++ "!"
      return (read pidStr :: Int)
    else do
      putStrLn $ "Error: Expected PID from server, got: " ++ firstLine
      return 0

  myPidRef <- newIORef pid
  initGame <- newIORef (GameState [[]] [] [] [] [] Lobby [] [] 0.0 0.0 0.0)
  typingRef <- newIORef False
  bufferRef <- newIORef ""
  visualsRef <- newIORef Map.empty

  let st = ClientState h initGame visualsRef typingRef bufferRef myPidRef gameAssets
  
  _ <- forkIO (recvLoop st)
  playIO
    (InWindow "Bomberman Client" (800, 600) (100, 100))
    black
    30
    st
    drawState
    handleInput
    updateFunc

-- moveTowards
moveTowards :: Float -> Float -> Float -> Float
moveTowards current target amount
  | current < target = min (current + amount) target
  | current > target = max (current - amount) target
  | otherwise        = target

-- updateFunc
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

-- updateVisualPlayer
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
-- drawState
drawState :: ClientState -> IO Picture
drawState ClientState{..} = do
  gs <- readIORef gameVar
  typing <- readIORef isTyping
  buffer <- readIORef chatBuffer
  visuals <- readIORef visualPlayers
  myPid <- readIORef myPlayerId

  let gamePic = drawGame assets gs visuals 

  let chatUI = if status gs /= Lobby
               then Pictures [ drawChatHistory (chatHistory gs)
                             , drawChatInput typing buffer ]
               else Blank

  let playerLabel = if status gs == Playing && myPid > 0
        then Translate (-380) 280 $ 
             Scale 0.15 0.15 $
             Color green $
             Text ("You are P" ++ show myPid)
        else Blank
  return (Pictures [gamePic, chatUI, playerLabel])
  
-- handleInput
handleInput :: Event -> ClientState -> IO ClientState
handleInput event st@ClientState{..} = do
  typing <- readIORef isTyping
  gs <- readIORef gameVar
  
  case status gs of
    Lobby -> handleLobby event st
    _     -> if typing
             then handleTyping event st
             else handlePlaying event st

-- handleLobby
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

-- handlePlaying
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

-- handleTyping
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
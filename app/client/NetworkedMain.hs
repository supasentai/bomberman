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

-- Import các module từ thư viện (trong 'src/')
import Types
import Render

-- ClientState (Giữ nguyên)
data ClientState = ClientState
  { connHandle :: Handle
  , gameVar    :: IORef GameState
  , isTyping   :: IORef Bool
  , chatBuffer :: IORef String
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

-- main (SỬA LỖI)
main :: IO ()
main = do
  h <- connectServer "127.0.0.1" "4242"
  
  -- SỬA LỖI: Thêm `30.0` (trường thứ 10) cho `gamePhaseTimer`
  initGame <- newIORef (GameState [[]] [] [] [] [] Playing [] [] 0.0 30.0)
  
  typingRef <- newIORef False
  bufferRef <- newIORef ""

  let st = ClientState h initGame typingRef bufferRef
  
  _ <- forkIO (recvLoop st)
  playIO
    (InWindow "Bomberman Client" (800, 600) (100, 100))
    black
    30
    st
    drawState
    handleInput
    (\_ -> return)

-- NÂNG CẤP: drawState (Sửa lỗi logic)
drawState :: ClientState -> IO Picture
drawState ClientState{..} = do
  -- 1. Lấy trạng thái game (từ server)
  gs <- readIORef gameVar
  -- 2. Lấy trạng thái gõ phím (NỘI BỘ client)
  typing <- readIORef isTyping
  buffer <- readIORef chatBuffer

  -- 3. Vẽ các thành phần
  --    drawGame (vẽ bản đồ, timer) dùng `gs`
  let gamePic = drawGame gs 
  
  -- SỬA LỖI LOGIC:
  --    drawChatHistory phải dùng `chatHistory` từ `gs`
  --    drawChatInput phải dùng `typing` và `buffer` từ `ClientState`
  let chatHistoryPic = drawChatHistory (chatHistory gs)
  let chatInputPic = drawChatInput typing buffer
  
  return (Pictures [gamePic, chatHistoryPic, chatInputPic])

-- handleInput (Giữ nguyên)
handleInput :: Event -> ClientState -> IO ClientState
handleInput event st@ClientState{..} = do
  typing <- readIORef isTyping
  
  if typing
  then handleTyping event st
  else handlePlaying event st

-- handlePlaying (Giữ nguyên)
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
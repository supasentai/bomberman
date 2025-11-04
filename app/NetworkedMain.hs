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
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  putStrLn "✅ Connected to Bomberman server!"
  return h

-- recvLoop (Giữ nguyên)
recvLoop :: ClientState -> IO ()
recvLoop st@ClientState{..} = forever $ do
  msgLine <- hGetLine connHandle
  let msg = BL.pack msgLine
  case decode msg of
    Just gs -> writeIORef gameVar gs
    Nothing -> putStrLn "⚠️ Parse error from server"

-- main (Giữ nguyên)
main :: IO ()
main = do
  h <- connectServer "127.0.0.1" "4242"
  
  initGame <- newIORef (GameState [[]] [] [] [] [] Playing [])
  
  typingRef <- newIORef False
  bufferRef <- newIORef ""

  let st = ClientState h initGame typingRef bufferRef
  
  _ <- forkIO (recvLoop st)
  playIO
    (InWindow "💣 Bomberman Client" (800, 600) (100, 100))
    black
    30
    st
    drawState
    handleInput
    (\_ -> return)

-- drawState (Giữ nguyên)
drawState :: ClientState -> IO Picture
drawState ClientState{..} = do
  gs <- readIORef gameVar
  typing <- readIORef isTyping
  buffer <- readIORef chatBuffer

  let gamePic = drawGame gs
  let chatHistoryPic = drawChatHistory (chatHistory gs)
  let chatInputPic = drawChatInput typing buffer
  
  return (Pictures [gamePic, chatHistoryPic, chatInputPic])

-- handleInput (GiGữ nguyên)
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

-- NÂNG CẤP: Xử lý Backspace (cả 2 kiểu)
handleTyping :: Event -> ClientState -> IO ClientState
handleTyping (EventKey (SpecialKey KeyEnter) Down _ _) st@ClientState{..} = do
  -- Nhấn Enter (gõ) -> Gửi tin nhắn
  buffer <- readIORef chatBuffer
  if not (null buffer)
  then do
    hPutStrLn connHandle ("/say " ++ buffer)
    hFlush connHandle
    writeIORef chatBuffer "" -- Xóa buffer
  else
    return ()
  
  writeIORef isTyping False -- Chuyển về chế độ chơi
  return st

handleTyping (EventKey (SpecialKey KeyBackspace) Down _ _) st@ClientState{..} = do
  -- SỬA LỖI 1: Xử lý `SpecialKey KeyBackspace`
  modifyIORef chatBuffer (\b -> if null b then "" else init b)
  return st

handleTyping (EventKey (Char '\b') Down _ _) st@ClientState{..} = do
  -- SỬA LỖI 2: Xử lý `Char '\b'` (Backspace trên một số hệ thống)
  modifyIORef chatBuffer (\b -> if null b then "" else init b)
  return st

handleTyping (EventKey (Char c) Down _ _) st@ClientState{..}
  | isPrint c = do -- Chỉ nhận các ký tự in được
      modifyIORef chatBuffer (\b -> b ++ [c])
      return st
handleTyping _ st = return st -- Bỏ qua các phím khác
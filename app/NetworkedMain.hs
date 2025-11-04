{-# LANGUAGE RecordWildCards #-}

module Main where -- (Giữ nguyên `Main` từ lần sửa trước)

import Graphics.Gloss.Interface.IO.Game
import Network.Socket
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.IORef
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import Render

-- | Client state giữ kết nối và game hiện tại
data ClientState = ClientState
  { connHandle :: Handle
  , gameVar    :: IORef GameState
  }

-- | Kết nối tới server
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

-- | Thread nhận dữ liệu liên tục từ server
recvLoop :: ClientState -> IO ()
recvLoop st@ClientState{..} = forever $ do
  msgLine <- hGetLine connHandle
  let msg = BL.pack msgLine
  case decode msg of
    Just gs -> writeIORef gameVar gs
    Nothing -> putStrLn "⚠️ Parse error from server"

-- | Hàm khởi chạy client (Gloss)
main :: IO ()
main = do
  h <- connectServer "127.0.0.1" "4242"
  

  initGame <- newIORef (GameState [[]] [] [] [] [] Playing)
  
  let st = ClientState h initGame
  _ <- forkIO (recvLoop st)
  playIO
    (InWindow "💣 Bomberman Client" (800, 600) (100, 100))
    black
    30
    st
    drawState
    handleInput
    (\_ -> return)

-- | Vẽ game bằng Gloss
drawState :: ClientState -> IO Picture
drawState ClientState{..} = do
  gs <- readIORef gameVar
  return (drawGame gs)

-- | Xử lý phím người chơi
handleInput :: Event -> ClientState -> IO ClientState
handleInput (EventKey (Char c) Down _ _) st@ClientState{..}
  | c `elem` ("wasd" :: String) = do
      hPutStrLn connHandle [c]
      hFlush connHandle
      return st
  | c == 'b' = do
      hPutStrLn connHandle "b"
      hFlush connHandle
      return st
handleInput _ st = return st
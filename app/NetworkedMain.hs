{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module NetworkedMain where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad (forever, when)
import Data.IORef
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Types
import Render

serverHost, serverPort :: String
serverHost = "127.0.0.1"
serverPort = "4242"

main :: IO ()
main = do
  putStr "Nhập player ID (1 hoặc 2): "
  pid <- readLn
  putStrLn "🎮 Starting Networked Bomberman Client..."
  addrinfos <- getAddrInfo Nothing (Just serverHost) (Just serverPort)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  putStrLn "✅ Connected to Bomberman Server (Gloss Mode)"

  stateVar <- newIORef initGameState

  -- Thread nhận dữ liệu từ server
  forkIO $ forever $ do
    msg <- BS.hGetLine h
    let json = BL.fromStrict msg
    case decode json :: Maybe GameState of
      Just gs -> writeIORef stateVar gs
      Nothing -> putStrLn "⚠️ Parse error from server"

  playIO
    (InWindow ("Player " ++ show pid) (600, 600) (100, 100))
    white
    120
    initGameState
    (\_ -> do gs <- readIORef stateVar
              drawGame gs)
    (handleInput h pid)
    (\_ w -> return w)

handleInput :: Handle -> Int -> Event -> GameState -> IO GameState
handleInput h pid (EventKey (Char c) Down _ _) gs = do
  when (c `elem` ("wasdb" :: String)) $
    BL.hPutStrLn h (encode (Command pid [toUpper c]))
  return gs
handleInput _ _ _ gs = return gs

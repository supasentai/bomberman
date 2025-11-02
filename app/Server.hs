{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Server where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Types
import GameLogic (movePlayer, dropBomb)

port :: String
port = "4242"

runServer :: IO ()
runServer = withSocketsDo $ do
  putStrLn "🖥️  Server running at port 4242"
  addrinfos <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] })
                           Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5

  gsVar <- newTVarIO initGameState

  forever $ do
    (conn, clientAddr) <- accept sock
    putStrLn $ "✅ Client connected: " ++ show clientAddr
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ handleClient h gsVar

handleClient :: Handle -> TVar GameState -> IO ()
handleClient h gsVar = forever $ do
  msg <- BS.hGetLine h
  let json = BL.fromStrict msg
  case decode json :: Maybe Command of
    Nothing -> putStrLn "⚠️ Invalid JSON"
    Just Command{..} -> atomically $ do
      gs <- readTVar gsVar
      let gs' = applyAction playerId action gs
      writeTVar gsVar gs'
  gsNew <- readTVarIO gsVar
  BL.hPutStrLn h (encode gsNew)

applyAction :: Int -> String -> GameState -> GameState
applyAction pid act gs =
  case act of
    "W" -> movePlayer pid (0,1) gs
    "S" -> movePlayer pid (0,-1) gs
    "A" -> movePlayer pid (-1,0) gs
    "D" -> movePlayer pid (1,0) gs
    "B" -> dropBomb pid gs
    _   -> gs

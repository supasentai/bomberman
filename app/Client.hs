{-# LANGUAGE OverloadedStrings #-}

module Client where

import Network.Socket
import System.IO
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (decode)
import Types (GameState(..), Player(..), Bomb(..))

serverHost :: String
serverHost = "127.0.0.1"
serverPort :: String
serverPort = "4242"

------------------------------------------------------------
runClient :: IO ()
runClient = withSocketsDo $ do
  putStrLn "🔗 Connecting to Bomberman Server..."
  addrinfos <- getAddrInfo Nothing (Just serverHost) (Just serverPort)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)

  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  putStrLn "✅ Connected! Use keys: W A S D B (press Enter after each key)"

  forever $ do
    putStr "> "
    cmd <- getLine
    hPutStrLn h cmd
    msg <- BS.hGetLine h
    let json = BL.fromStrict msg
    case decode json :: Maybe GameState of
      Nothing -> putStrLn "⚠️  Could not parse server response."
      Just gs -> do
        let (x, y) = pos (player gs)
            bcount = length (bombs gs)
        putStrLn $ "📍 Player: (" ++ show x ++ "," ++ show y ++ "), 💣 Bombs: " ++ show bcount

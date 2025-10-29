{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Types
import GameLogic (updateGame, canMove)

------------------------------------------------------------
-- | Cấu hình server
------------------------------------------------------------
port :: String
port = "4242"

------------------------------------------------------------
-- | Chạy server
------------------------------------------------------------
runServer :: IO ()
runServer = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just defaultHints {addrFlags = [AI_PASSIVE]})
                           Nothing (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  listen sock 2
  putStrLn $ "🖥️  Server running at port " ++ port

  state <- newTVarIO initGame
  forever $ do
    (conn, addr) <- accept sock
    putStrLn $ "✅ Client connected: " ++ show addr
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ handleClient h state

------------------------------------------------------------
-- | Mỗi client có 1 thread xử lý
------------------------------------------------------------
handleClient :: Handle -> TVar GameState -> IO ()
handleClient h state = forever $ do
  msg <- hGetLine h
  atomically $ do
    gs <- readTVar state
    let gs' = applyAction msg gs
    writeTVar state gs'
  gsNew <- readTVarIO state
  BL.hPutStrLn h (encode gsNew)

------------------------------------------------------------
-- | Xử lý hành động từ client
------------------------------------------------------------
applyAction :: String -> GameState -> GameState
applyAction "W" = move (0,-1)
applyAction "S" = move (0,1)
applyAction "A" = move (-1,0)
applyAction "D" = move (1,0)
applyAction "B" = dropBomb
applyAction _   = id

------------------------------------------------------------
-- | Di chuyển người chơi
------------------------------------------------------------
move :: (Int, Int) -> GameState -> GameState
move (dx, dy) gs@GameState{..} =
  let (x, y) = pos player
      newPos = (x + dx, y + dy)
  in if canMove board newPos
       then gs { player = player { pos = newPos } }
       else gs

------------------------------------------------------------
-- | Đặt bom
------------------------------------------------------------
dropBomb :: GameState -> GameState
dropBomb gs@GameState{..} =
  let p = player
      already = any ((== pos p) . bPos) bombs
  in if already
        then gs
        else gs { bombs = Bomb (pid p) (pos p) 3 : bombs }

------------------------------------------------------------
-- | Game ban đầu
------------------------------------------------------------
initGame :: GameState
initGame = GameState
  { board  = replicate 10 (replicate 10 Empty)
  , player = Player 1 (1,1) True
  , bombs  = []
  , tick   = 0
  }

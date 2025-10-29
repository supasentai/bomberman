module Main where

import Types
import GameLogic
import Render
import Control.Concurrent (threadDelay)
import System.IO (hSetEncoding, stdout, utf8)

------------------------------------------------------------
-- Khởi tạo map và state
------------------------------------------------------------
board15 :: Board
board15 =
  [ replicate 15 Wall
  , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
  , replicate 15 Wall
  ]

p1 = Player 1 (2,2) Human 3
p2 = Player 2 (12,9) AI 3

initGame = GameState board15 [p1,p2] [Bomb 1 (4,4) 5 2, Bomb 2 (8,6) 3 2] 0

------------------------------------------------------------
-- Main loop tự động
------------------------------------------------------------
main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "Starting Bomberman demo (no input)..."
  gameLoop initGame

------------------------------------------------------------
-- Game loop
------------------------------------------------------------
gameLoop :: GameState -> IO ()
gameLoop gs = do
  renderGame gs
  threadDelay 500000  -- 0.5s mỗi frame
  let newGs = updateGame gs
  gameLoop newGs

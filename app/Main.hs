module Main (main) where

import Types
import GameLogic
import System.IO (hSetEncoding, stdout, utf8)

-- Hàm tạo bản đồ mẫu (7x7)
sampleBoard :: Board
sampleBoard =
  [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  ]

-- Hai người chơi
p1 :: Player
p1 = Player { pid = 1, pos = (2, 2), pType = Human, hp = 3 }

p2 :: Player
p2 = Player { pid = 2, pos = (4, 2), pType = AI, hp = 3 }

-- GameState ban đầu
initGame :: GameState
initGame = GameState
  { board = sampleBoard
  , players = [p1, p2]
  , bombs = []
  , tick = 0
  }

-- Hàm main: test di chuyển và đặt bom
main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "==== TEST GAMELOGIC ===="
  print initGame

  let p1Moved = movePlayer p1 MoveRight
  putStrLn "\n▶ Player 1 di chuyển sang phải:"
  print p1Moved

  let bombs1 = placeBomb p1Moved []
  putStrLn "\n▶ Player 1 đặt bom:"
  print bombs1

  let gs1 = initGame { bombs = bombs1 }
  let gs2 = updateGame gs1
  putStrLn "\n▶ Game sau 1 tick:"
  print gs2

  let gs3 = iterate updateGame gs1 !! 4
  putStrLn "\n▶ Game sau 4 tick (bom nổ):"
  print gs3

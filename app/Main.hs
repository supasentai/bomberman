module Main where

import Control.Concurrent (threadDelay)
import Render
import Types
import System.IO (hSetEncoding, stdout, utf8)



-- Tạo bản đồ mẫu (7x7)
sampleBoard :: Board
sampleBoard =
  [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, SoftBlock, Empty, Flame, Empty, SoftBlock, Wall]
  , [Wall, Empty, Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, SoftBlock, Empty, SoftBlock, Empty, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  ]

-- Hai người chơi + 1 bom
samplePlayers :: [Player]
samplePlayers =
  [ Player { pid = 1, pos = (1,1), pType = Human }
  , Player { pid = 2, pos = (5,1), pType = AI }
  ]

sampleBombs :: [Bomb]
sampleBombs = [ Bomb { owner = 1, bPos = (3,2), timer = 2 } ]

-- Tạo GameState mẫu
sampleGame :: GameState
sampleGame = GameState
  { board   = sampleBoard
  , players = samplePlayers
  , bombs   = sampleBombs
  , tick    = 0
  }

-- Hàm main: hiển thị liên tục mô phỏng game
main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn "Chạy demo Bomberman..."
    loop sampleGame 0

loop :: GameState -> Int -> IO ()
loop game n = do
    let game' = game { tick = n }
    renderGame game'
    threadDelay 30000

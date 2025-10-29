{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import GameLogic
import Render

------------------------------------------------------------
-- Tạo board 13x11 với border tường
-- y = 0 là hàng TRÊN, y tăng xuống
-- x = 0 là cột TRÁI, x tăng sang phải
------------------------------------------------------------
initBoard :: Board
initBoard =
  topRow
  ++ middleRows
  ++ bottomRow
  where
    w = 13  -- số cột
    h = 11  -- số hàng

    topRow    = [replicate w Wall]  -- hàng 0 toàn Wall
    bottomRow = [replicate w Wall]  -- hàng cuối toàn Wall

    -- các hàng ở giữa: Wall ở viền trái/phải
    middleRows =
      [ [ Wall
        ] ++ [ cellFor x y
             | x <- [1 .. w-2]
             ]
        ++ [ Wall ]
      | y <- [1 .. h-2]
      ]

    -- quy tắc ô:
    -- cứ (even x && even y) thì Wall cứng
    -- còn lại SoftBlock
    cellFor x y
      | even x && even y = Wall
      | otherwise        = SoftBlock

------------------------------------------------------------
-- Trạng thái ban đầu
------------------------------------------------------------
initPlayer :: Player
initPlayer = Player
  { pid   = 1
  , pos   = (1,1)   -- nhớ: (x,y) trong lưới, y=0 là hàng trên
  , alive = True
  }

initGame :: GameState
initGame = GameState
  { board  = carveSpawn initBoard
  , player = initPlayer
  , bombs  = []
  , tick   = 0
  }

-- Dọn bớt SoftBlock vùng spawn để người chơi không bị kẹt
carveSpawn :: Board -> Board
carveSpawn b =
  carve (1,1) $
  carve (1,2) $
  carve (2,1) b
  where
    carve (x,y) bd =
      [ [ if (ix,iy)==(x,y) && (c==SoftBlock || c==Flame)
            then Empty else c
        | (ix,c) <- zip [0..] row]
      | (iy,row) <- zip [0..] bd
      ]

------------------------------------------------------------
-- Vẽ khung hình
------------------------------------------------------------
drawWorld :: GameState -> IO Picture
drawWorld = drawGame

------------------------------------------------------------
-- Điều khiển bàn phím
------------------------------------------------------------
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (Char 'w') Down _ _) gs = pure (tryMove ( 0,-1) gs)
handleInput (EventKey (Char 's') Down _ _) gs = pure (tryMove ( 0, 1) gs)
handleInput (EventKey (Char 'a') Down _ _) gs = pure (tryMove (-1, 0) gs)
handleInput (EventKey (Char 'd') Down _ _) gs = pure (tryMove ( 1, 0) gs)

handleInput (EventKey (Char 'b') Down _ _) gs = pure (dropBomb gs)

handleInput _ gs = pure gs

------------------------------------------------------------
-- Mỗi frame (dt giây)
------------------------------------------------------------
updateWorld :: Float -> GameState -> IO GameState
updateWorld dt gs = pure (updateGame dt gs)

------------------------------------------------------------
-- Thử di chuyển người chơi thêm (dx,dy) ô lưới
-- Lưu ý: trong logic, y tăng xuống => 'w' là (0,-1), 's' là (0,+1)
------------------------------------------------------------
tryMove :: (Int,Int) -> GameState -> GameState
tryMove (dx,dy) gs@GameState{..} =
  let (x,y)    = pos player
      newPos   = (x+dx, y+dy)
  in if canMove board newPos
        then gs { player = player { pos = newPos } }
        else gs

------------------------------------------------------------
-- Đặt bom tại vị trí người chơi (timer 3 giây)
------------------------------------------------------------
dropBomb :: GameState -> GameState
dropBomb gs@GameState{..} =
  let ppos = pos player
      already = any (\b -> bPos b == ppos) bombs
  in if already
        then gs
        else gs { bombs = Bomb (pid player) ppos 3 : bombs }

------------------------------------------------------------
-- Main
------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Bomberman Gloss: W A S D để di chuyển, B để đặt bom."
  playIO
    (InWindow "Bomberman Gloss" (900, 700) (100, 100))
    white
    30          -- FPS
    initGame
    drawWorld
    handleInput
    updateWorld

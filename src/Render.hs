module Render (drawGame) where

import Graphics.Gloss
import Types

-- Kích thước một ô (pixel)
cellSize :: Float
cellSize = 40

-- Chuyển tọa độ (ô lưới x,y) → toạ độ vẽ Gloss (px,py)
-- Quy ước logic:
--   (0,0) = ô góc TRÊN-TRÁI của board
-- Gloss:
--   (0,0) = giữa màn hình, y tăng lên
gridToScreen
  :: Board
  -> (Int, Int)   -- (x,y) trong lưới
  -> (Float,Float)
gridToScreen b (gx, gy) =
  ( x0 + fromIntegral gx * cellSize + half
  , y0 - fromIntegral gy * cellSize - half
  )
  where
    w  = length (head b)
    h  = length b
    wPix = fromIntegral w * cellSize
    hPix = fromIntegral h * cellSize
    x0 = - wPix / 2
    y0 =   hPix / 2
    half = cellSize / 2

-- Vẽ toàn bộ game
drawGame :: GameState -> IO Picture
drawGame gs = pure $
  Pictures
    [ drawBoard (board gs)
    , drawBombs (board gs) (bombs gs)
    , drawPlayer (board gs) (player gs)
    ]

-- Vẽ nền + ô
drawBoard :: Board -> Picture
drawBoard b =
  Pictures [ drawCell b (x,y) c
           | (y,row) <- zip [0..] b
           , (x,c)   <- zip [0..] row
           ]

drawCell :: Board -> (Int,Int) -> Cell -> Picture
drawCell b (gx,gy) c =
  let (sx, sy) = gridToScreen b (gx,gy)
      tile col = translate sx sy $ color col (rectangleSolid cellSize cellSize)
  in case c of
      Wall      -> tile (greyN 0.2)
      SoftBlock -> tile (greyN 0.5)
      Empty     -> tile (greyN 0.85)
      Flame     -> tile red

-- Vẽ người chơi ở đúng ô
drawPlayer :: Board -> Player -> Picture
drawPlayer b p =
  let (gx,gy) = pos p
      (sx,sy) = gridToScreen b (gx,gy)
  in translate sx sy $
        color blue (circleSolid (cellSize * 0.35))

-- Vẽ bom
drawBombs :: Board -> [Bomb] -> Picture
drawBombs b bs =
  Pictures [ let (sx,sy) = gridToScreen b (bPos bomb)
             in translate sx sy $
                  color black (circleSolid (cellSize * 0.25))
           | bomb <- bs
           ]

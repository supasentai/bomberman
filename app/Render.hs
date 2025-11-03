module Render where

import Graphics.Gloss
import Types
import Debug.Trace

cellSize :: Float
cellSize = 40

-- SỬA CẢNH BÁO: Dùng 'case' để xử lý `board` an toàn
drawGame :: GameState -> Picture
drawGame gs =
  case board gs of
    [] -> Translate 0 0 $ Scale 0.2 0.2 $ Text "Board is empty"
    b@(firstRow:_) -> -- Dùng `b` và `firstRow`
      Translate (-w'/2) (-h'/2 + 100) $
      Scale 1.2 1.2 $
      Pictures $
        concat
          [ [drawBoard b] -- `b` là board
          , drawPlayers h (players gs)
          , drawBombs h (bombs gs)
          , drawFlames h (flames gs)
          , drawPowerUps h (powerups gs)
          ]
      where
        w = length firstRow -- An toàn
        h = length b        -- An toàn
        w' = fromIntegral w * cellSize
        h' = fromIntegral h * cellSize

drawBoard :: Board -> Picture
drawBoard b =
  Pictures
    [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
        (drawCell c)
    | (y, row) <- zip [0..] (reverse b)
    , (x, c) <- zip [0..] row
    ]

drawCell :: Cell -> Picture
drawCell Wall  = color (greyN 0.2) $ rectangleSolid cellSize cellSize
drawCell Box   = color (greyN 0.4) $ rectangleSolid cellSize cellSize
drawCell Empty = color (greyN 0.7) $ rectangleSolid cellSize cellSize

drawPlayers :: Int -> [Player] -> [Picture]
drawPlayers h ps =
  [ translate (fromIntegral x * cellSize)
              (fromIntegral (h - 1 - y) * cellSize)
      $ Pictures
          [ color (dark (greyN 0.1)) (translate 3 (-3) (circleSolid r))
          , color col (circleSolid r)
          ]
  | Player pid (x,y) True _ _ <- ps
  , let col = if pid == 1 then red else blue
  , let r = cellSize / 3
  ]

drawBombs :: Int -> [Bomb] -> [Picture]
drawBombs h bs =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ Scale s s
      $ color (dark red) (thickCircle (cellSize/3) 6)
  | Bomb (x,y) t _ _ <- bs
  , let s = 0.8 + 0.4 * (t / 3)
  ]

drawFlames :: Int -> [Flame] -> [Picture]
drawFlames h fs =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ color (makeColorI 255 180 0 (round (255 * alpha)))
      $ rectangleSolid (cellSize*0.9) (cellSize*0.9)
  | Flame (x,y) r <- fs
  , let alpha = max 0 (r / 1.5)
  ]

drawPowerUps :: Int -> [PowerUp] -> [Picture]
drawPowerUps h pups =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ Pictures
          [ color (makeColorI 255 255 255 40) $ rectangleSolid (cellSize*0.8) (cellSize*0.8)
          , color c $ circleSolid (cellSize / 4)
          ]
  | PowerUp (x,y) pType <- pups
  , let c = case pType of
              BombUp  -> yellow
              FlameUp -> orange
  ]
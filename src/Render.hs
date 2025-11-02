module Render where

import Graphics.Gloss
import Types
import Debug.Trace

cellSize :: Float
cellSize = 40

drawGame :: GameState -> Picture
drawGame gs =
  Translate (-w'/2) (-h'/2 + 100) $
  Scale 1.2 1.2 $
  Pictures $
    concat
      [ [drawBoard (board gs)]
      , drawPlayers (players gs)
      , drawBombs (bombs gs)
      , drawFlames (flames gs)
      ]
  where
    b = board gs
    w = length (head b)
    h = length b
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

drawPlayers :: [Player] -> [Picture]
drawPlayers ps =
  [ translate (fromIntegral x * cellSize)
              (fromIntegral y * cellSize)
      $ Pictures
          [ color (dark (greyN 0.1)) (translate 3 (-3) (circleSolid r))
          , color col (circleSolid r)
          ]
  | Player pid (x,y) True <- ps
  , let col = if pid == 1 then red else blue
  , let r = cellSize / 3
  ]

drawBombs :: [Bomb] -> [Picture]
drawBombs bs =
  [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
      $ Scale s s
      $ color (dark red) (thickCircle (cellSize/3) 6)
  | Bomb (x,y) t <- bs
  , let s = 0.8 + 0.4 * (t / 3)
  ]

drawFlames :: [Flame] -> [Picture]
drawFlames fs =
  [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
      $ color (makeColorI 255 180 0 (round (255 * alpha)))
      $ rectangleSolid (cellSize*0.9) (cellSize*0.9)
  | Flame (x,y) r <- fs
  , let alpha = max 0 (r / 1.5)
  ]

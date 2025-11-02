{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types

-- Di chuyển player có id = pid'
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid' (dx, dy) gs@GameState{..} =
  let updated = map moveOne players
  in gs { players = updated }
  where
    moveOne p
      | pid p == pid' =
          let (x,y) = pos p
          in p { pos = (x+dx, y+dy) }
      | otherwise = p

-- Đặt bom tại vị trí player
dropBomb :: Int -> GameState -> GameState
dropBomb pid' gs@GameState{..} =
  case [p | p <- players, pid p == pid'] of
    (p:_) -> gs { bombs = pos p : bombs }
    []    -> gs

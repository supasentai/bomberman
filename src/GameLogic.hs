{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types

-- Cấu hình chung
bombDefaultTimer :: Float
bombDefaultTimer = 3.0

flameDuration :: Float
flameDuration = 1.5

-- Kiểm tra ô có hợp lệ không
canMove :: Board -> (Int, Int) -> Bool
canMove b (x, y)
  | x < 0 || y < 0 = False
  | y >= length b || x >= length (head b) = False
  | otherwise =
      case (b !! y) !! x of
        Wall -> False
        Box  -> False
        _    -> True

-- Di chuyển player
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid (dx, dy) gs@GameState{..} =
  let ps = map updatePlayer players
  in gs { players = ps }
  where
    updatePlayer p
      | playerId p == pid =
          let (x, y) = pos p
              newPos = (x + dx, y + dy)
          in if canMove board newPos then p { pos = newPos } else p
      | otherwise = p


-- Đặt bom
dropBomb :: Int -> GameState -> GameState
dropBomb pid gs =
  case [p | p@(Player i _ a) <- players gs, i == pid, a] of
    [] -> gs
    (Player _ (x, y) _ : _) ->
      gs { bombs = Bomb (x, y) bombDefaultTimer : bombs gs }

-- Cập nhật game theo thời gian
tickGame :: Float -> GameState -> GameState
tickGame dt gs =
  gs { bombs = bombs', flames = flames' }
  where
    bombs'  = [ Bomb p (t - dt) | Bomb p t <- bombs gs, t - dt > 0 ]
    flames' =
      [ Flame p (r - dt) | Flame p r <- flames gs, r - dt > 0 ]
      ++ [ Flame p flameDuration | Bomb p t <- bombs gs, t - dt <= 0 ]

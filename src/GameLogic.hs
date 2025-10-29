module GameLogic where

import Types
import Data.List (nub)

------------------------------------------------------------
-- Kiểm tra có thể đi được (chưa dùng, để sẵn)
------------------------------------------------------------
canMove :: Board -> (Int, Int) -> Bool
canMove b (x, y)
  | y < 0 || y >= length b         = False
  | null b || x < 0 || x >= length (head b)  = False
  | otherwise =
      case (b !! y) !! x of
        Empty -> True
        Flame -> True
        _     -> False

------------------------------------------------------------
-- Cập nhật bom, flame, tick
------------------------------------------------------------
tickBombs :: [Bomb] -> [Bomb]
tickBombs = map (\b -> b { timer = timer b - 1 })

explodedBombs :: [Bomb] -> [Bomb]
explodedBombs = filter ((<= 0) . timer)

explodeBomb :: Bomb -> [(Int, Int)]
explodeBomb b =
  let (x, y) = bPos b
      p = power b
  in nub $
      [(x, y)] ++
      [(x + dx, y) | dx <- [-p..p], dx /= 0] ++
      [(x, y + dy) | dy <- [-p..p], dy /= 0]

------------------------------------------------------------
-- Cập nhật game mỗi tick
------------------------------------------------------------
updateGame :: GameState -> GameState
updateGame gs =
  let bs1 = tickBombs (bombs gs)
      expBombs = explodedBombs bs1
      flames = concatMap explodeBomb expBombs
      nb = applyFlames (board gs) flames
      remain = filter ((>0) . timer) bs1
  in gs { board = nb, bombs = remain, tick = tick gs + 1 }

------------------------------------------------------------
-- Tạo lửa và làm nguội
------------------------------------------------------------
applyFlames :: Board -> [(Int, Int)] -> Board
applyFlames b cs =
  [ [ cellUpdate x y c | (x, c) <- zip [0..] row ]
  | (y, row) <- zip [0..] b
  ]
  where
    cellUpdate x y c
      | (x, y) `elem` cs =
          case c of
            SoftBlock -> Flame
            Empty     -> Flame
            _         -> c
      | otherwise = decayFlame c

decayFlame :: Cell -> Cell
decayFlame Flame = Empty
decayFlame c     = c

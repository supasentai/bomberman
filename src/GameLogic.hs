module GameLogic where

import Types
import Data.List (nub)

canMove :: Board -> (Int, Int) -> Bool
canMove b (x, y)
  | y < 0 || y >= length b         = False
  | null b || x < 0 || x >= length (head b) = False
  | otherwise =
      case (b !! y) !! x of
        Empty -> True
        Flame -> True  -- cho phép đi vào flame, tùy bạn
        _     -> False

tickBombs :: Float -> [Bomb] -> [Bomb]
tickBombs dt = map (\b -> b { timer = timer b - dt })

explodedBombs :: [Bomb] -> [Bomb]
explodedBombs = filter ((<= 0) . timer)

explodeBomb :: Bomb -> [(Int, Int)]
explodeBomb b =
  let (x, y) = bPos b
      power = 2 :: Int
      lineX = [ (x+dx,y) | dx <- [-power..power], dx /= 0 ]
      lineY = [ (x,y+dy) | dy <- [-power..power], dy /= 0 ]
  in nub $ (x,y) : lineX ++ lineY

applyFlames :: Board -> [(Int, Int)] -> Board
applyFlames b cellsOnFire =
  [ [ applyCell (x,y) c
    | (x,c) <- zip [0..] row ]
  | (y,row) <- zip [0..] b
  ]
  where
    applyCell (x,y) c
      | (x,y) `elem` cellsOnFire =
          case c of
            SoftBlock -> Flame
            Empty     -> Flame
            _         -> c
      | otherwise = decayFlame c

decayFlame :: Cell -> Cell
decayFlame Flame = Empty
decayFlame c     = c

updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let bs1 = tickBombs dt (bombs gs)
      expNow = explodedBombs bs1
      flames = concatMap explodeBomb expNow
      nb = applyFlames (board gs) flames
      stillTicking = filter ((>0) . timer) bs1
  in gs { board = nb
        , bombs = stillTicking
        , tick  = tick gs + dt
        }

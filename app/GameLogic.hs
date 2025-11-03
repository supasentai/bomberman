{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL, find)
import Data.Maybe (isJust, fromMaybe)
import System.Random (StdGen, randomR)

-- Cấu hình
bombDefaultTimer :: Float
bombDefaultTimer = 3.0
flameDuration :: Float
flameDuration = 1.5
powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5 -- 50% cơ hội

-- canMove
canMove :: Board -> (Int, Int) -> Bool
canMove b (x, y)
  | x < 0 || y < 0 = False
  | y >= length b || x >= length (head b) = False
  | otherwise =
      case (b !! y) !! x of
        Wall -> False
        Box  -> False
        _    -> True

-- HÀM MỚI: Áp dụng vật phẩm cho người chơi
applyPowerUp :: Player -> PowerUpType -> Player
applyPowerUp p BombUp  = p { maxBombs = maxBombs p + 1 }
applyPowerUp p FlameUp = p { blastRadius = blastRadius p + 1 }

-- NÂNG CẤP: movePlayer giờ xử lý nhặt vật phẩm
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid (dx, dy) gs@GameState{..} =
  let (collectedAnything', ps') = mapAccumL updatePlayer False players
      
      gs' = gs { players = ps' }
      
      (Player _ newPos _ _ _) = head [p | p <- ps', playerId p == pid]
      
  in if collectedAnything'
     then gs' { powerups = filter (\p -> pupPos p /= newPos) powerups }
     else gs'
  
  where
    updatePlayer :: Bool -> Player -> (Bool, Player)
    updatePlayer collected p
      | playerId p == pid =
          let (x, y) = pos p
              newPos = (x + dx, y + dy)
          in if canMove board newPos 
             then 
                  let p' = p { pos = newPos }
                      mPowerUp = find (\pu -> pupPos pu == newPos) powerups
                  in case mPowerUp of
                       Nothing -> (collected, p')
                       Just pup -> (True, applyPowerUp p' (pupType pup))
             else (collected, p)
      | otherwise = (collected, p)


-- NÂNG CẤP: dropBomb giờ kiểm tra maxBombs
dropBomb :: Int -> GameState -> GameState
dropBomb pid gs@GameState{..} =
  case find (\p -> playerId p == pid && alive p) players of
    Nothing -> gs
    Just p ->
      let 
        (x, y) = pos p
        myBombs = length [b | b <- bombs, bOwner b == pid]
      in
        if myBombs < maxBombs p
        then gs { bombs = Bomb (x, y) bombDefaultTimer (blastRadius p) pid : bombs }
        else gs

-- safeGetCell
safeGetCell :: Board -> (Int, Int) -> Maybe Cell
safeGetCell b (x, y)
  | y < 0 || y >= length b = Nothing
  | x < 0 || x >= length (head b) = Nothing
  | otherwise = Just ((b !! y) !! x)

-- getFlamesInDir
getFlamesInDir :: Board -> (Int, Int) -> (Int, Int) -> Int -> [Flame]
getFlamesInDir board (x, y) (dx, dy) n
  | n <= 0 = []
  | otherwise =
      let newPos = (x + dx, y + dy)
      in case safeGetCell board newPos of
           Nothing    -> []
           Just Wall  -> []
           Just Box   -> [Flame newPos flameDuration]
           Just Empty -> (Flame newPos flameDuration) : getFlamesInDir board newPos (dx, dy) (n - 1)

-- NÂNG CẤP: calculateExplosion dùng bRadius từ Bomb
calculateExplosion :: Board -> Bomb -> [Flame]
calculateExplosion board b@(Bomb pos _ radius _) =
  let
    (x, y) = pos
    r = radius
    center = [Flame pos flameDuration]
    up     = getFlamesInDir board pos ( 0, -1) r
    down   = getFlamesInDir board pos ( 0,  1) r
    left   = getFlamesInDir board pos (-1,  0) r
    right  = getFlamesInDir board pos ( 1,  0) r
  in
    center ++ up ++ down ++ left ++ right

-- NÂNG CẤP: tickGame dùng StdGen để tạo vật phẩm
tickGame :: Float -> StdGen -> GameState -> (GameState, StdGen)
tickGame dt rng gs@GameState{..} =
  let
    bombs'  = [ b { timer = timer b - dt } | b <- bombs, timer b - dt > 0 ]
    flames' = [ f { remain = remain f - dt } | f <- flames, remain f - dt > 0 ]
    explodingBombs = [ b | b <- bombs, timer b - dt <= 0 ]
    newFlames = concatMap (calculateExplosion board) explodingBombs
    allFlames = flames' ++ newFlames
    flamePositions = [fpos f | f <- allFlames]

    (newBoard, destroyedBoxPos) = updateBoard board flamePositions
    (newPowerUps, newRng) = createPowerUps rng destroyedBoxPos
    allPowerUps = powerups ++ newPowerUps
    newPlayers = updatePlayers players flamePositions

    gs' = gs { board = newBoard, 
               players = newPlayers, 
               bombs = bombs', 
               flames = allFlames, 
               powerups = allPowerUps }
  in
    (gs', newRng)

-- HÀM MỚI: Tạo vật phẩm ngẫu nhiên (ĐÃ SỬA LỖI)
createPowerUps :: StdGen -> [(Int, Int)] -> ([PowerUp], StdGen)
createPowerUps rng [] = ([], rng)
createPowerUps rng (pos:xs) =
  let
    (roll, rng') = randomR (0.0, 1.0) rng
    -- SỬA LỖI: Thêm (:: Int) để GHC biết đây là số nguyên
    (pType, rng'') = randomR (0 :: Int, 1 :: Int) rng'
    
    (remainingPowerUps, finalRng) = createPowerUps rng'' xs
    
    thisPowerUp = if roll < powerUpSpawnChance
                  -- 'pType' bây giờ đã rõ là Int, so sánh '==' hoạt động
                  then Just (PowerUp pos (if pType == 0 then BombUp else FlameUp))
                  else Nothing
  in
    case thisPowerUp of
      Just p  -> (p : remainingPowerUps, finalRng)
      Nothing -> (remainingPowerUps, finalRng)

-- updatePlayers
updatePlayers :: [Player] -> [(Int, Int)] -> [Player]
updatePlayers ps flamePositions =
  map updatePlayer ps
  where
    updatePlayer p
      | pos p `elem` flamePositions && alive p = p { alive = False }
      | otherwise = p

-- NÂNG CẤP: updateBoard giờ trả về vị trí các hòm đã vỡ
updateBoard :: Board -> [(Int, Int)] -> (Board, [(Int, Int)])
updateBoard b flamePositions =
  let
    (_, processedRows) = mapAccumL processRow 0 b
    newBoard = map fst processedRows
    allDestroyed = concatMap snd processedRows
  in
    (newBoard, allDestroyed)
  
  where
    processRow :: Int -> [Cell] -> (Int, ([Cell], [(Int, Int)]))
    processRow y row =
      let 
        (_, processedCells) = mapAccumL (processCell y) 0 row
        newRow = map fst processedCells
        destroyedPos = concatMap snd processedCells
      in 
        (y+1, (newRow, destroyedPos))

    processCell :: Int -> Int -> Cell -> (Int, (Cell, [(Int, Int)]))
    processCell y x cell
      | (x, y) `elem` flamePositions =
          case cell of
            Box  -> (x+1, (Empty, [(x,y)]))
            _    -> (x+1, (cell, []))
      | otherwise = (x+1, (cell, []))
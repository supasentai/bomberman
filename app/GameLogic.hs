{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL, find, filter)
import Data.Maybe (isJust, fromMaybe)
import System.Random (StdGen, randomR)

-- Cấu hình (Giữ nguyên)
bombDefaultTimer :: Float
bombDefaultTimer = 3.0
flameDuration :: Float
flameDuration = 1.5
powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5

-- NÂNG CẤP: `canMove` nhận `GameState`
-- SỬA LỖI: Đã thêm `gs@GameState{..}`
canMove :: GameState -> (Int, Int) -> Bool
canMove gs@GameState{..} (x, y)
  -- 1. Kiểm tra biên
  | x < 0 || y < 0 = False
  | y >= length board || x >= length (head board) = False
  | otherwise =
      let 
        -- 2. Kiểm tra tường/hòm (giờ `board` là 1 biến)
        cell = (board !! y) !! x
        isWallOrBox = case cell of
                        Wall -> True
                        Box  -> True
                        _    -> False
        
        -- 3. KIỂM TRA BOM (giờ `bombs` là 1 biến)
        bombAtPos = any (\b -> bpos b == (x, y)) bombs

      in 
        -- Chỉ có thể đi khi KHÔNG phải tường/hòm VÀ KHÔNG có bom
        not isWallOrBox && not bombAtPos

-- applyPowerUp (Giữ nguyên)
applyPowerUp :: Player -> PowerUpType -> Player
applyPowerUp p BombUp  = p { maxBombs = maxBombs p + 1 }
applyPowerUp p FlameUp = p { blastRadius = blastRadius p + 1 }

-- NÂNG CẤP: `movePlayer` gọi `canMove gs` (Giữ nguyên từ lần trước)
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
          in 
             if canMove gs newPos
             then 
                  let p' = p { pos = newPos }
                      mPowerUp = find (\pu -> pupPos pu == newPos) powerups
                  in case mPowerUp of
                       Nothing -> (collected, p')
                       Just pup -> (True, applyPowerUp p' (pupType pup))
             else (collected, p)
      | otherwise = (collected, p)

-- dropBomb (Giữ nguyên)
dropBomb :: Int -> GameState -> GameState
dropBomb pid gs@GameState{..} =
  case find (\p -> playerId p == pid && alive p) players of
    Nothing -> gs
    Just p ->
      let (x, y) = pos p
          myBombs = length [b | b <- bombs, bOwner b == pid]
      in if myBombs < maxBombs p
         then gs { bombs = Bomb (x, y) bombDefaultTimer (blastRadius p) pid : bombs }
         else gs

-- safeGetCell (Giữ nguyên)
safeGetCell :: Board -> (Int, Int) -> Maybe Cell
safeGetCell b (x, y)
  | y < 0 || y >= length b = Nothing
  | x < 0 || x >= length (head b) = Nothing
  | otherwise = Just ((b !! y) !! x)

-- getFlamesInDir (Giữ nguyên)
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

-- calculateExplosion (Giữ nguyên)
calculateExplosion :: Board -> Bomb -> [Flame]
calculateExplosion board b@(Bomb pos _ radius _) =
  let (x, y) = pos
      r = radius
      center = [Flame pos flameDuration]
      up     = getFlamesInDir board pos ( 0, -1) r
      down   = getFlamesInDir board pos ( 0,  1) r
      left   = getFlamesInDir board pos (-1,  0) r
      right  = getFlamesInDir board pos ( 1,  0) r
  in center ++ up ++ down ++ left ++ right

-- checkGameStatus (Giữ nguyên)
checkGameStatus :: [Player] -> GameStatus
checkGameStatus livingPlayers =
  case livingPlayers of
    []  -> Draw
    [p] -> GameOver (playerId p)
    _   -> Playing

-- tickGame (Giữ nguyên)
tickGame :: Float -> StdGen -> GameState -> (GameState, StdGen)
tickGame dt rng gs@GameState{..} =
  case status of
    GameOver _ -> (gs, rng)
    Draw       -> (gs, rng)
    Playing    -> 
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
        livingPlayers = filter alive newPlayers
        newStatus = checkGameStatus livingPlayers

        gs' = gs { board    = newBoard, 
                   players  = newPlayers, 
                   bombs    = bombs', 
                   flames   = allFlames, 
                   powerups = allPowerUps,
                   status   = newStatus
                 }
      in
        (gs', newRng)

-- createPowerUps (Giữ nguyên)
createPowerUps :: StdGen -> [(Int, Int)] -> ([PowerUp], StdGen)
createPowerUps rng [] = ([], rng)
createPowerUps rng (pos:xs) =
  let
    (roll, rng') = randomR (0.0, 1.0) rng
    (pType, rng'') = randomR (0 :: Int, 1 :: Int) rng'
    (remainingPowerUps, finalRng) = createPowerUps rng'' xs
    thisPowerUp = if roll < powerUpSpawnChance
                  then Just (PowerUp pos (if pType == 0 then BombUp else FlameUp))
                  else Nothing
  in
    case thisPowerUp of
      Just p  -> (p : remainingPowerUps, finalRng)
      Nothing -> (remainingPowerUps, finalRng)

-- updatePlayers (Giữ nguyên)
updatePlayers :: [Player] -> [(Int, Int)] -> [Player]
updatePlayers ps flamePositions =
  map updatePlayer ps
  where
    updatePlayer p
      | pos p `elem` flamePositions && alive p = p { alive = False }
      | otherwise = p

-- updateBoard (Giữ nguyên)
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
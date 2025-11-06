{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL, find, filter, partition)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import System.Random (StdGen, randomR)
import Control.Parallel.Strategies (parList, rseq, using)

-- Cấu hình
bombDefaultTimer :: Float
bombDefaultTimer = 3.0

-- SỬA LỖI (CẢI TIẾN 3): Giảm thời gian lửa tồn tại
flameDuration :: Float
flameDuration = 0.8 -- Giảm từ 1.5

powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5

-- canMove (Giữ nguyên)
canMove :: GameState -> (Int, Int) -> Bool
canMove gs@GameState{..} (x, y) =
  case board of
    [] -> False
    (firstRow:_) ->
      let width = length firstRow
          height = length board
      in
        x >= 0 && y >= 0 && x < width && y < height &&
        let 
          cell = (board !! y) !! x
          isWallOrBox = case cell of
                          Wall -> True
                          Box  -> True
                          _    -> False
          bombAtPos = any (\b -> bpos b == (x, y)) bombs
          monsterAtPos = any (\m -> mPos m == (x, y)) monsters
        in 
          not isWallOrBox && not bombAtPos && not monsterAtPos

-- applyPowerUp (Giữ nguyên)
applyPowerUp :: Player -> PowerUpType -> Player
applyPowerUp p BombUp  = p { maxBombs = maxBombs p + 1 }
applyPowerUp p FlameUp = p { blastRadius = blastRadius p + 1 }

-- movePlayer (Giữ nguyên)
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid (dx, dy) gs@GameState{..} =
  let (collectedAnything', ps') = mapAccumL updatePlayer False players
      gs' = gs { players = ps' }
  in
    case find (\p -> playerId p == pid) ps' of
      Nothing -> gs'
      Just (Player _ newPos _ _ _) ->
        if collectedAnything'
        then gs' { powerups = filter (\p -> pupPos p /= newPos) powerups }
        else gs'
  
  where
    updatePlayer :: Bool -> Player -> (Bool, Player)
    updatePlayer collected p
      | playerId p == pid =
          let (x, y) = pos p
              newPos = (x + dx, y + dy)
          in if canMove gs newPos 
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
safeGetCell b (x, y) =
  case b of
    [] -> Nothing
    (firstRow:_) ->
      let width = length firstRow
          height = length b
      in
        if x < 0 || y < 0 || x >= width || y >= height
        then Nothing
        else Just ((b !! y) !! x)

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

-- calculateSingleExplosion (Giữ nguyên)
calculateSingleExplosion :: Board -> Bomb -> [Flame]
calculateSingleExplosion board b@(Bomb pos _ radius _) =
  let (x, y) = pos
      r = radius
      center = [Flame pos flameDuration]
      up     = getFlamesInDir board pos ( 0, -1) r
      down   = getFlamesInDir board pos ( 0,  1) r
      left   = getFlamesInDir board pos (-1,  0) r
      right  = getFlamesInDir board pos ( 1,  0) r
  in center ++ up ++ down ++ left ++ right

-- calculateAllExplosions (Giữ nguyên)
calculateAllExplosions :: Board -> [Bomb] -> [Bomb] -> ([Flame], [Bomb])
calculateAllExplosions _ [] remainingBombs = ([], remainingBombs)
calculateAllExplosions board explodingBombs remainingBombs =
  let
    currentFlames = concatMap (calculateSingleExplosion board) explodingBombs
    flamePositions = map fpos currentFlames
    (triggeredBombs, safeBombs) = 
        partition (\b -> bpos b `elem` flamePositions) remainingBombs
    (chainedFlames, finalBombs) = 
        calculateAllExplosions board triggeredBombs safeBombs
  in
    (currentFlames ++ chainedFlames, finalBombs)

-- checkGameStatus (Giữ nguyên)
checkGameStatus :: [Player] -> GameStatus
checkGameStatus livingPlayers =
  case livingPlayers of
    []  -> Draw
    [p] -> GameOver (playerId p)
    _   -> Playing

-- HÀM TRỢ GIÚP (Giữ nguyên)
updateBombTimer :: Float -> Bomb -> Maybe Bomb
updateBombTimer dt b = 
  let t = timer b - dt
  in if t > 0 then Just (b { timer = t }) else Nothing

updateFlameTimer :: Float -> Flame -> Maybe Flame
updateFlameTimer dt f = 
  let r = remain f - dt
  in if r > 0 then Just (f { remain = r }) else Nothing

-- tickGame (Giữ nguyên)
tickGame :: Float -> StdGen -> GameState -> (GameState, StdGen)
tickGame dt rng gs@GameState{..} =
  case status of
    GameOver _ -> (gs, rng)
    Draw       -> (gs, rng)
    Playing    -> 
      let
        updatedBombs = map (updateBombTimer dt) bombs `using` parList rseq
        bombsToKeep = catMaybes updatedBombs
        updatedFlames = map (updateFlameTimer dt) flames `using` parList rseq
        flames' = catMaybes updatedFlames
        (explodingBombs, remainingBombs) = 
            partition (\b -> timer b - dt <= 0) bombsToKeep
        (newFlames, bombsAfterExplosion) = 
            calculateAllExplosions board explodingBombs remainingBombs
        allFlames = flames' ++ newFlames
        flamePositions = [fpos f | f <- allFlames]
        
        (newBoard, destroyedBoxPos) = updateBoard board flamePositions
        (newPowerUps, newRng) = createPowerUps rng destroyedBoxPos
        allPowerUps = powerups ++ newPowerUps
        
        newMonsters = monsters 
        newPlayers = updatePlayers players flamePositions newMonsters
        livingPlayers = filter alive newPlayers
        newStatus = checkGameStatus livingPlayers

        gs' = gs { board    = newBoard, 
                   players  = newPlayers,
                   bombs    = bombsAfterExplosion,
                   flames   = allFlames, 
                   powerups = allPowerUps,
                   status   = newStatus,
                   monsters = newMonsters
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
updatePlayers :: [Player] -> [(Int, Int)] -> [Monster] -> [Player]
updatePlayers ps flamePositions monsterPositions =
  map updatePlayer ps
  where
    monsterPos = map mPos monsterPositions
    updatePlayer p
      | alive p && (pos p `elem` flamePositions || pos p `elem` monsterPos) = 
          p { alive = False }
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
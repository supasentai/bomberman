{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL, find, filter, partition, sortBy)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import System.Random (StdGen, randomR)
import Control.Parallel.Strategies (parList, rseq, using)

-- Imports cho BFS (Giữ nguyên)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..))
import Data.Foldable (toList)

-- Cấu hình (Giữ nguyên)
bombDefaultTimer :: Float
bombDefaultTimer = 3.0
flameDuration :: Float
flameDuration = 0.8
powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5
monsterMoveSpeed :: Float
monsterMoveSpeed = 0.5 

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

-- ========== LOGIC AI TÌM ĐƯỜNG (ĐÃ NÂNG CẤP) ==========

-- 1. Tìm mục tiêu (Giữ nguyên)
findClosestPlayer :: (Int, Int) -> [Player] -> Maybe (Int, Int)
findClosestPlayer mPos allPlayers =
  let livingPlayers = filter alive allPlayers
      dist (x, y) (px, py) = abs (x - px) + abs (y - py)
      sortedPlayers = sortBy (\p1 p2 -> compare (dist mPos (pos p1)) (dist mPos (pos p2))) livingPlayers
  in case sortedPlayers of
      []    -> Nothing
      (p:_) -> Just (pos p)

-- 2. Thuật toán BFS (Giữ nguyên)
findPath :: Board -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findPath board obstacles start end =
  case board of
    [] -> Nothing
    (firstRow:_) -> 
      let
        width = length firstRow
        height = length board
        isWalkable :: (Int, Int) -> Bool
        isWalkable p@(x, y) =
          x >= 0 && y >= 0 && x < width && y < height &&
          (case (board !! y) !! x of
             Wall -> False
             Box  -> False
             _    -> True) &&
          not (p `elem` obstacles)

        bfs :: Seq ((Int, Int), (Int, Int)) -> Set (Int, Int) -> Maybe (Int, Int)
        bfs queue visited
          | Seq.null queue = Nothing
          | otherwise =
              case Seq.viewl queue of
                EmptyL -> Nothing
                ((currPos, firstStep) :< rest) ->
                  if currPos == end
                  then Just firstStep
                  else
                    let (cx, cy) = currPos
                        neighbors = filter (\p -> isWalkable p && not (Set.member p visited))
                                       [(cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1)]
                        newVisited = Set.union visited (Set.fromList neighbors)
                        updateStep n = if currPos == start then n else firstStep
                        newQueue = foldl (\q n -> q |> ((n, updateStep n))) rest neighbors
                    in
                      bfs newQueue newVisited
      in
        bfs (Seq.singleton (start, start)) (Set.singleton start)

-- HÀM MỚI: Logic "Lang thang" (Wander)
-- Thử di chuyển 4 hướng theo thứ tự
wander :: GameState -> Monster -> Monster
wander gs m =
  let
    (x, y) = mPos m
    -- Thử 4 hướng theo thứ tự ưu tiên
    possibleMoves = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
    
    -- Lấy hướng đi hợp lệ đầu tiên
    -- `canMove` sẽ tự động kiểm tra (tường, bom, quái vật khác)
    validMove = find (canMove gs) possibleMoves
  in
    case validMove of
      Nothing -> m -- Bị kẹt hoàn toàn, đứng yên
      Just pos -> m { mPos = pos } -- Di chuyển "lang thang"

-- NÂNG CẤP: `calculateMonsterMove` giờ sẽ "lang thang"
calculateMonsterMove :: GameState -> Monster -> Monster
calculateMonsterMove gs m =
  let 
    mPos' = mPos m
    targetPos = findClosestPlayer mPos' (players gs)
    obstacles = (map bpos (bombs gs)) ++ 
                (map mPos (filter (\m' -> mId m' /= mId m) (monsters gs)))
  in
    case targetPos of
      -- SỬA: Nếu không có người chơi, đi lang thang
      Nothing -> wander gs m
      Just target ->
        -- Tính toán (tác vụ nặng)
        case findPath (board gs) obstacles mPos' target of
          -- SỬA: Nếu không tìm thấy đường, đi lang thang
          Nothing -> wander gs m
          Just nextStep -> m { mPos = nextStep } -- Có đường, di chuyển
-- ========== KẾT THÚC LOGIC AI ==========


-- NÂNG CẤP: tickGame (Giữ nguyên từ lần trước)
tickGame :: Float -> StdGen -> GameState -> (GameState, StdGen)
tickGame dt rng gs@GameState{..} =
  case status of
    GameOver _ -> (gs, rng)
    Draw       -> (gs, rng)
    Playing    -> 
      let
        -- 1. SONG SONG: Cập nhật timer (Bom/Lửa)
        updatedBombs = map (updateBombTimer dt) bombs `using` parList rseq
        bombsToKeep = catMaybes updatedBombs
        updatedFlames = map (updateFlameTimer dt) flames `using` parList rseq
        flames' = catMaybes updatedFlames

        -- 2. NỔ DÂY CHUYỀN
        (explodingBombs, remainingBombs) = 
            partition (\b -> timer b - dt <= 0) bombsToKeep
        (newFlames, bombsAfterExplosion) = 
            calculateAllExplosions board explodingBombs remainingBombs
        allFlames = flames' ++ newFlames
        flamePositions = [fpos f | f <- allFlames]
        
        -- 3. CẬP NHẬT BẢN ĐỒ VÀ VẬT PHẨM
        (newBoard, destroyedBoxPos) = updateBoard board flamePositions
        (newPowerUps, newRng) = createPowerUps rng destroyedBoxPos
        allPowerUps = powerups ++ newPowerUps
        
        -- 4. CẬP NHẬT AI (ĐÃ GIẢM TỐC)
        (monsterMoves, newMonsterTimer) =
          if monsterMoveTimer <= 0
          then
            (map (calculateMonsterMove gs) monsters `using` parList rseq, monsterMoveSpeed)
          else
            (monsters, monsterMoveTimer - dt) 
        
        -- 5. LỌC QUÁI VẬT SỐNG SÓT
        survivingMonsters = filter (\m -> mPos m `notElem` flamePositions) monsterMoves
        
        -- 6. CẬP NHẬT NGƯỜI CHƠI VÀ TRẠNG THÁI
        newPlayers = updatePlayers players flamePositions survivingMonsters
        livingPlayers = filter alive newPlayers
        newStatus = checkGameStatus livingPlayers

        gs' = gs { board    = newBoard, 
                   players  = newPlayers,
                   bombs    = bombsAfterExplosion,
                   flames   = allFlames, 
                   powerups = allPowerUps,
                   status   = newStatus,
                   monsters = survivingMonsters,
                   monsterMoveTimer = newMonsterTimer
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
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

-- Cấu hình
bombDefaultTimer :: Float
bombDefaultTimer = 3.0
flameDuration :: Float
flameDuration = 0.8
powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5
monsterMoveSpeed :: Float
monsterMoveSpeed = 0.5 
chaosDuration :: Float
chaosDuration = 10.0
chaosBombTimer :: Float
chaosBombTimer = 0.5 

-- MỚI: Cấu hình cho Khiên
invincibilityDuration :: Float
invincibilityDuration = 1.0 -- 1 giây bất tử sau khi khiên vỡ

-- canMove (Giữ nguyên)
canMove :: GameState -> Player -> (Int, Int) -> Bool
canMove gs@GameState{..} p (x, y) =
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
          isBlockedByBomb = bombAtPos && (chaosTimer p <= 0)
          monsterAtPos = any (\m -> mPos m == (x, y)) monsters
        in 
          not isWallOrBox && not isBlockedByBomb && not monsterAtPos

-- applyPowerUp (Giữ nguyên)
applyPowerUp :: Player -> PowerUpType -> Player
applyPowerUp p BombUp  = p { maxBombs = maxBombs p + 1 }
applyPowerUp p FlameUp = p { blastRadius = blastRadius p + 1 }
applyPowerUp p Shield  = p { hasShield = True }
applyPowerUp p Chaos   = p { chaosTimer = chaosDuration }

-- movePlayer (Giữ nguyên)
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid (dx, dy) gs@GameState{..} =
  let (collectedAnything', ps') = mapAccumL updatePlayer False players
      gs' = gs { players = ps' }
  in
    case find (\p -> playerId p == pid) ps' of
      Nothing -> gs'
      Just (Player _ newPos _ _ _ _ _ _) -> -- Sửa pattern match
        if collectedAnything'
        then gs' { powerups = filter (\p -> pupPos p /= newPos) powerups }
        else gs'
  
  where
    updatePlayer :: Bool -> Player -> (Bool, Player)
    updatePlayer collected p
      | playerId p == pid =
          let (x, y) = pos p
              newPos = (x + dx, y + dy)
          in 
             if canMove gs p newPos
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
          bombTimer = if chaosTimer p > 0
                      then chaosBombTimer
                      else bombDefaultTimer
      in 
        if myBombs < maxBombs p
        then gs { bombs = Bomb (x, y) bombTimer (blastRadius p) pid : bombs }
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
        
-- spawnItemNearPlayer (Giữ nguyên)
spawnItemNearPlayer :: Int -> String -> GameState -> GameState
spawnItemNearPlayer pid itemStr gs@GameState{..} =
  let mItemType = case itemStr of
                    "bombup"  -> Just BombUp
                    "flameup" -> Just FlameUp
                    "shield"  -> Just Shield
                    "chaos"   -> Just Chaos
                    _         -> Nothing
      mPlayer = find (\p -> playerId p == pid) players
  in
    case (mItemType, mPlayer) of
      (Just itemType, Just p) ->
        let (x, y) = pos p
            spots = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
            isSpotFree (sx, sy) =
              case safeGetCell board (sx, sy) of
                Just Empty -> not (any (\pu -> pupPos pu == (sx, sy)) powerups)
                _          -> False
            mValidSpot = find isSpotFree spots
        in
          case mValidSpot of
            Nothing -> gs
            Just spot -> 
              let newPowerUp = PowerUp spot itemType
              in gs { powerups = newPowerUp : powerups }
      _ -> gs

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

-- NÂNG CẤP: `updatePlayerTimers` (Thêm `iframes`)
updatePlayerTimers :: Float -> [Player] -> [Player]
updatePlayerTimers dt = map update
  where
    update p = 
      let 
        -- Đếm ngược Chaos
        newChaos = if chaosTimer p > 0
                   then max 0 (chaosTimer p - dt)
                   else 0
        -- Đếm ngược iframes
        newIframes = if iframes p > 0
                     then max 0 (iframes p - dt)
                     else 0
      in
        p { chaosTimer = newChaos, iframes = newIframes }

-- ========== LOGIC AI TÌM ĐƯỜNG (Giữ nguyên) ==========
aiIsWalkable :: GameState -> MonsterType -> (Int, Int) -> Bool
aiIsWalkable gs@GameState{..} mType (x, y) =
  case board of
    [] -> False
    (firstRow:_) ->
      let width = length firstRow
          height = length board
      in
        x >= 0 && y >= 0 && x < width && y < height &&
        (case (board !! y) !! x of
           Wall -> False
           Box  -> case mType of
                     Grunt -> False
                     Ghost -> True
           _    -> True) &&
        not (any (\b -> bpos b == (x, y)) bombs) &&
        not (any (\m -> mPos m == (x, y)) monsters)

findClosestPlayer :: (Int, Int) -> [Player] -> Maybe (Int, Int)
findClosestPlayer mPos allPlayers =
  let livingPlayers = filter alive allPlayers
      dist (x, y) (px, py) = abs (x - px) + abs (y - py)
      sortedPlayers = sortBy (\p1 p2 -> compare (dist mPos (pos p1)) (dist mPos (pos p2))) livingPlayers
  in case sortedPlayers of
      []    -> Nothing
      (p:_) -> Just (pos p)

findPath :: GameState -> MonsterType -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findPath gs@GameState{..} mType obstacles start end =
  case board of
    [] -> Nothing
    (firstRow:_) -> 
      let
        width = length firstRow
        height = length board
        isWalkable = aiIsWalkable (gs { monsters = [] }) mType
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

wander :: GameState -> Monster -> Monster
wander gs m@(Monster _ (x,y) mType') =
  let
    possibleMoves = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
    validMove = find (aiIsWalkable gs mType') possibleMoves
  in
    case validMove of
      Nothing -> m
      Just pos -> m { mPos = pos }

calculateMonsterMove :: GameState -> Monster -> Monster
calculateMonsterMove gs m@(Monster _ mPos' mType') =
  let 
    targetPos = findClosestPlayer mPos' (players gs)
    obstacles = (map bpos (bombs gs)) ++ 
                (map mPos (filter (\m' -> mId m' /= mId m) (monsters gs)))
  in
    case targetPos of
      Nothing -> wander gs m
      Just target ->
        case findPath gs mType' obstacles mPos' target of
          Nothing -> wander gs m
          Just nextStep -> m { mPos = nextStep }
-- ========== KẾT THÚC LOGIC AI ==========


-- NÂNG CẤP: tickGame (Thứ tự thực thi)
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
        
        -- 4. NÂNG CẤP AI: Tiến hóa
        (evolvedMonsters, newPhaseTimer) =
          if gamePhaseTimer > 0
          then
               let newTimer = gamePhaseTimer - dt
               in if newTimer <= 0
                  then (map (\m -> m { mType = Ghost }) monsters, 0.0)
                  else (monsters, newTimer)
          else (monsters, 0.0)
        
        -- 5. CẬP NHẬT AI (ĐÃ GIẢM TỐC)
        (monsterMoves, newMonsterTimer) =
          if monsterMoveTimer <= 0
          then
            (map (calculateMonsterMove gs {monsters = evolvedMonsters, players = players}) evolvedMonsters `using` parList rseq, monsterMoveSpeed)
          else
            (evolvedMonsters, monsterMoveTimer - dt) 
        
        -- 6. LỌC QUÁI VẬT SỐNG SÓT
        survivingMonsters = filter (\m -> mPos m `notElem` flamePositions) monsterMoves
        
        -- 7. CẬP NHẬT NGƯỜI CHƠI (Xử lý chết)
        playersAfterHits = updatePlayers players flamePositions survivingMonsters
        
        -- 8. CẬP NHẬT TIMERS (SAU KHI ĐÃ NHẶT ĐỒ/CHẾT)
        newPlayers = updatePlayerTimers dt playersAfterHits
        
        livingPlayers = filter alive newPlayers
        newStatus = checkGameStatus livingPlayers

        gs' = gs { board    = newBoard, 
                   players  = newPlayers,
                   bombs    = bombsAfterExplosion,
                   flames   = allFlames, 
                   powerups = allPowerUps,
                   status   = newStatus,
                   monsters = survivingMonsters,
                   monsterMoveTimer = newMonsterTimer,
                   gamePhaseTimer = newPhaseTimer
                 }
      in
        (gs', newRng)

-- NÂNG CẤP: `createPowerUps` (Giữ nguyên)
createPowerUps :: StdGen -> [(Int, Int)] -> ([PowerUp], StdGen)
createPowerUps rng [] = ([], rng)
createPowerUps rng (pos:xs) =
  let
    (roll, rng') = randomR (0.0, 1.0) rng
    (pTypeIdx, rng'') = randomR (0 :: Int, 3 :: Int) rng'
    (remainingPowerUps, finalRng) = createPowerUps rng'' xs
    thisPowerUp = if roll < powerUpSpawnChance
                  then let pType = case pTypeIdx of
                                     0 -> BombUp
                                     1 -> FlameUp
                                     2 -> Shield
                                     _ -> Chaos
                       in Just (PowerUp pos pType)
                  else Nothing
  in
    case thisPowerUp of
      Just p  -> (p : remainingPowerUps, finalRng)
      Nothing -> (remainingPowerUps, finalRng)

-- NÂNG CẤP: `updatePlayers` (Logic bất tử/khiên hoàn chỉnh)
updatePlayers :: [Player] -> [(Int, Int)] -> [Monster] -> [Player]
updatePlayers ps flamePositions monsterPositions =
  map updatePlayer ps
  where
    monsterPos = map mPos monsterPositions
    isHit p = alive p && (pos p `elem` flamePositions || pos p `elem` monsterPos)
    
    updatePlayer p
      | isHit p = -- Nếu bị va chạm
          if iframes p > 0 || chaosTimer p > 0
          then p -- Bất tử (từ Chaos hoặc iframes)
          else -- Không bất tử, kiểm tra Khiên
            if hasShield p
            then p { hasShield = False, iframes = invincibilityDuration } -- Mất khiên, BẬT iframes
            else p { alive = False } -- Chết
      | otherwise = p -- Không bị va chạm

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
{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL, find, filter, partition, sortBy, foldl')
import Data.Maybe (isJust, fromMaybe, catMaybes)
import System.Random (StdGen, randomR)
import Control.Parallel.Strategies (parList, rseq, using)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..))
import Data.Foldable (toList)

bombDefaultTimer :: Float
bombDefaultTimer = 2.0
flameDuration :: Float
flameDuration = 0.5
powerUpSpawnChance :: Float
powerUpSpawnChance = 0.5
monsterMoveSpeed :: Float
monsterMoveSpeed = 0.5 
chaosDuration :: Float
chaosDuration = 10.0
chaosBombTimer :: Float
chaosBombTimer = 0.5 
invincibilityDuration :: Float
invincibilityDuration = 1.0
playerAIMoveSpeed :: Float
playerAIMoveSpeed = 0.2

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

-- applyPowerUp
applyPowerUp :: Player -> PowerUpType -> Player
applyPowerUp p BombUp  = p { maxBombs = maxBombs p + 1 }
applyPowerUp p FlameUp = p { blastRadius = blastRadius p + 1 }
applyPowerUp p Shield  = p { hasShield = True }
applyPowerUp p Chaos   = p { chaosTimer = chaosDuration }

-- movePlayer
movePlayer :: Int -> (Int, Int) -> GameState -> GameState
movePlayer pid (dx, dy) gs@GameState{..} =
  let (collectedAnything', ps') = mapAccumL updatePlayer False players
      gs' = gs { players = ps' }
  in
    case find (\p -> playerId p == pid) ps' of
      Nothing -> gs'
      Just (Player _ newPos _ _ _ _ _ _ _) ->
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

-- dropBomb
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

-- safeGetCell
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
        
-- spawnItemNearPlayer
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

-- calculateSingleExplosion
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

-- calculateAllExplosions
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

-- checkGameStatus
checkGameStatus :: [Player] -> [Monster] -> GameStatus
checkGameStatus players monsters =
  let 
    livingPlayers = filter alive players
  in
    case livingPlayers of
      []  -> Draw 
      [p] -> GameOver (playerId p) 
      _   -> Playing 

-- updateBombTimer
updateBombTimer :: Float -> Bomb -> Maybe Bomb
updateBombTimer dt b = 
  let t = timer b - dt
  in if t > 0 then Just (b { timer = t }) else Nothing

-- updateFlameTimer
updateFlameTimer :: Float -> Flame -> Maybe Flame
updateFlameTimer dt f = 
  let r = remain f - dt
  in if r > 0 then Just (f { remain = r }) else Nothing

-- updatePlayerTimers
updatePlayerTimers :: Float -> [Player] -> [Player]
updatePlayerTimers dt = map update
  where
    update p = 
      let 
        newChaos = if chaosTimer p > 0
                   then max 0 (chaosTimer p - dt)
                   else 0
        newIframes = if iframes p > 0
                     then max 0 (iframes p - dt)
                     else 0
      in
        p { chaosTimer = newChaos, iframes = newIframes }

-- ========== LOGIC AI ==========

-- aiIsWalkable
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

-- findClosestPlayer
findClosestPlayer :: (Int, Int) -> [Player] -> Maybe (Int, Int)
findClosestPlayer mPos allPlayers =
  let livingPlayers = filter alive allPlayers
      dist (x, y) (px, py) = abs (x - px) + abs (y - py)
      sortedPlayers = sortBy (\p1 p2 -> compare (dist mPos (pos p1)) (dist mPos (pos p2))) livingPlayers
  in case sortedPlayers of
      []    -> Nothing
      (p:_) -> Just (pos p)
-- findPath 
findPath :: GameState -> MonsterType -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findPath gs mType obstacles start end =
  case board gs of
    [] -> Nothing
    (firstRow:_) -> 
      let
        width = length firstRow
        height = length (board gs)
        isWalkable pos = aiIsWalkable (gs { monsters = [] }) mType pos &&
                         not (pos `elem` obstacles)
        
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

findPathToSafety :: GameState -> Player -> Set (Int, Int) -> Maybe (Int, Int)
findPathToSafety gs p dangerTiles =
  let 
    start@(startX, startY) = pos p
    isWalkableForEscape (x,y) = canMove gs p (x,y)

    bfs :: Seq ((Int, Int), (Int, Int)) -> Set (Int, Int) -> Maybe (Int, Int)
    bfs queue visited
      | Seq.null queue = Nothing 
      | otherwise =
          case Seq.viewl queue of
            EmptyL -> Nothing
            ((currPos, firstStep) :< rest) ->
              if not (currPos `Set.member` dangerTiles)
              then 
                let (nextX, nextY) = firstStep
                in Just (nextX - startX, nextY - startY)
              else
                let (cx, cy) = currPos
                    neighbors = filter (\pos -> isWalkableForEscape pos && not (Set.member pos visited))
                                   [(cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1)]
                    newVisited = Set.union visited (Set.fromList neighbors)
                    updateStep n = if currPos == start then n else firstStep
                    newQueue = foldl (\q n -> q |> ((n, updateStep n))) rest neighbors
                in
                  bfs newQueue newVisited
  in
    bfs (Seq.singleton (start, start)) (Set.singleton start)

-- wander
wander :: GameState -> Monster -> Monster
wander gs m@(Monster _ (x,y) mType') =
  let
    possibleMoves = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
    validMove = find (aiIsWalkable gs mType') possibleMoves
  in
    case validMove of
      Nothing -> m
      Just pos -> m { mPos = pos }

-- calculateMonsterMove
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

-- ========== HÀM MỚI: LOGIC AI CHO PLAYER ==========

--Định nghĩa các hành động mà AI Player có thể thực hiện
data AIAction = AIWait | AIMove (Int, Int) | AIDropBomb

calculateExplosionPath :: Board -> Bomb -> [(Int, Int)]
calculateExplosionPath board b =
    map fpos (calculateSingleExplosion board b)

getDangerTiles :: GameState -> Set (Int, Int)
getDangerTiles gs =
  let
    flamePositions = Set.fromList $ map fpos (flames gs)
    bombPositions = concatMap (calculateExplosionPath (board gs)) (bombs gs)
  in
    Set.union flamePositions (Set.fromList bombPositions)

findSafeWander :: GameState -> Player -> Set (Int, Int) -> StdGen -> ((Int, Int), StdGen)
findSafeWander gs p dangerTiles rng =
  let 
    (x, y) = pos p
    possibleMoves = [(0, -1), (0, 1), (-1, 0), (1, 0)]
    validMoves = filter (\(dx,dy) -> canMove gs p (x+dx, y+dy)) possibleMoves
    safeMoves = filter (\(dx,dy) -> not ((x+dx, y+dy) `Set.member` dangerTiles)) validMoves
  in
    if null safeMoves
    then ((0, 0), rng) 
    else 
      let (idx, newRng) = randomR (0, length safeMoves - 1) rng
      in (safeMoves !! idx, newRng)

--tìm mục tiêu gần nhất
findClosestTarget :: GameState -> Player -> Maybe (Int, Int)
findClosestTarget gs p =
  let 
    myPos = pos p
    powerUpPos = map pupPos (powerups gs)
    enemyPos = [pos p' | p' <- players gs, playerId p' /= playerId p, alive p', not (isAI p')]
    
    allTargets = powerUpPos ++ enemyPos
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    
  in
    if null allTargets
    then Nothing
    else 
      let 
        sortedTargets = sortBy (\t1 t2 -> compare (dist myPos t1) (dist myPos t2)) allTargets
      in
        Just (head sortedTargets)

--AI săn đuổi mục tiêu một cách an toàn
aiStrategicHunt :: GameState -> Player -> Set (Int, Int) -> StdGen -> ((Int, Int), StdGen)
aiStrategicHunt gs p dangerTiles rng =
  let 
    (x, y) = pos p
    mTargetPos = findClosestTarget gs p
    
    obstacles = (map bpos (bombs gs)) ++ 
                (map mPos (monsters gs)) ++
                (Set.toList dangerTiles)
    
    mPath = case mTargetPos of
              Nothing -> Nothing
              Just target -> findPath gs Grunt obstacles (pos p) target
  in
    case mPath of
      Just (nextX, nextY) -> ((nextX - x, nextY - y), rng) 
      Nothing -> findSafeWander gs p dangerTiles rng 

--Quyết định hành động dựa trên GameState
decideAIAction :: GameState -> Player -> StdGen -> (AIAction, StdGen)
decideAIAction gs p rng =
  let 
    (x, y) = pos p
    pid = playerId p
    
    --BƯỚC 1: NHẬN THỨC MỐI NGUY HIỂM
    dangerTiles = getDangerTiles gs 
    
  in
    --BƯỚC 2: ƯU TIÊN AN TOÀN (NÉ)
    if (x, y) `Set.member` dangerTiles && chaosTimer p <= 0
    then 
        case findPathToSafety gs p dangerTiles of
          Just moveDir -> (AIMove moveDir, rng) 
          Nothing -> (AIWait, rng)
        
    --BƯỚC 3: TẤN CÔNG (NẾU AN TOÀN)
    else
      let
        -- Kiểm tra các ô xung quanh xem có mục tiêu không
        adjacentTiles = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        isBox (tx, ty) = safeGetCell (board gs) (tx, ty) == Just Box
        isEnemy (tx, ty) = any (\pl -> pos pl == (tx, ty) && not (isAI pl) && alive pl) (players gs)
        
        -- Kiểm tra cả quái vật
        isMonster (tx, ty) = any (\m -> mPos m == (tx, ty)) (monsters gs)
        
        -- Mục tiêu giờ bao gồm cả quái vật
        isTarget t = isBox t || isEnemy t || isMonster t
        
        bombableTargets = filter isTarget adjacentTiles
        
        (roll, r1) = randomR (0.0 :: Float, 1.0 :: Float) rng
        -- AI sẽ đặt bom nếu có mục tiêu (bao gồm quái vật)
        shouldBomb = not (null bombableTargets) && roll < 0.7 
      in
        if shouldBomb
        then (AIDropBomb, r1) -- AI sẽ tự né vào tick sau nhờ logic ở BƯỚC 2
        
        --BƯỚC 4: DI CHUYỂN (SĂN ĐUỔI hoặc LANG THANG)
        else 
            let (moveRoll, r2) = randomR (0.0 :: Float, 1.0 :: Float) r1
                (moveDir, r_final)
                  | moveRoll < 0.9 = aiStrategicHunt gs p dangerTiles r2 
                  | otherwise      = findSafeWander gs p dangerTiles r2 
            in (AIMove moveDir, r_final)
      
--Áp dụng hành động cho 1 AI Player
applyAIAction :: (GameState, StdGen) -> Player -> (GameState, StdGen)
applyAIAction (gs, rng) p =
  let (action, newRng) = decideAIAction gs p rng
  in case action of
       AIWait -> (gs, newRng)
       AIMove dir -> (movePlayer (playerId p) dir gs, newRng)
       AIDropBomb -> (dropBomb (playerId p) gs, newRng)

--Chạy AI cho tất cả Player nào là AI và còn sống
processPlayerAI :: GameState -> StdGen -> (GameState, StdGen)
processPlayerAI gs rng =
  let aiPlayers = filter (\p -> isAI p && alive p) (players gs)
  in foldl' (applyAIAction) (gs, rng) aiPlayers


-- ========== KẾT THÚC LOGIC AI CHO PLAYER ==========


--tickGame
tickGame :: Float -> StdGen -> GameState -> (GameState, StdGen)
tickGame dt rng gs =
  case status gs of
    GameOver _ -> (gs, rng)
    Draw       -> (gs, rng)
    Playing    -> 
      let
        --BƯỚC 1: TÍNH TOÁN AI 
        -- 1a. Tinh Monster AI 
        (gsAfterMonsterAI, newMonsterTimer) =
          if monsterMoveTimer gs <= 0 
          then
            let 
              newTimer = monsterMoveSpeed
              gsForMonsterAI = gs { monsters = monsters gs } 
              monsterMoves = map (calculateMonsterMove gsForMonsterAI) (monsters gs) `using` parList rseq 
              gsAfterMonsterAI' = gs { monsters = monsterMoves }
            in (gsAfterMonsterAI', newTimer)
          else
            (gs, monsterMoveTimer gs - dt)
        
        -- 1b. Tinh Player AI 
        (gsAfterPlayerAI, newPlayerAITimer, newRngAfterAI) =
          if playerAIMoveTimer gsAfterMonsterAI <= 0
          then
            let
              newTimer = playerAIMoveSpeed
              (gsAfterPlayerAI', newRng') = processPlayerAI gsAfterMonsterAI rng
            in (gsAfterPlayerAI', newTimer, newRng')
          else
            (gsAfterMonsterAI, playerAIMoveTimer gsAfterMonsterAI - dt, rng)

        --TỪ BƯỚC 2 TRỞ ĐI: Dùng gsAfterPlayerAI làm trạng thái cơ sở
        -- 2. Cap nhat timer cho bom
        updatedBombs = map (updateBombTimer dt) (bombs gsAfterPlayerAI) `using` parList rseq
        bombsToKeep = catMaybes updatedBombs
        updatedFlames = map (updateFlameTimer dt) (flames gsAfterPlayerAI) `using` parList rseq
        flames' = catMaybes updatedFlames
        
        -- 3. Tinh toan bom no
        (explodingBombs, remainingBombs) = 
            partition (\b -> timer b - dt <= 0) bombsToKeep
        (newFlames, bombsAfterExplosion) = 
            calculateAllExplosions (board gsAfterPlayerAI) explodingBombs remainingBombs
        allFlames = flames' ++ newFlames
        flamePositions = [fpos f | f <- allFlames]
        
        -- 4. Cap nhat Board va Powerup
        (newBoard, destroyedBoxPos) = updateBoard (board gsAfterPlayerAI) flamePositions
        (newPowerUps, newRng') = createPowerUps newRngAfterAI destroyedBoxPos
        allPowerUps = (powerups gsAfterPlayerAI) ++ newPowerUps
        
        -- 5. Quai vat tien hoa
        (evolvedMonsters, newPhaseTimer) =
          if gamePhaseTimer gs > 0
          then
               let newTimer = gamePhaseTimer gs - dt
               in if newTimer <= 0
                  then (map (\m -> m { mType = Ghost }) (monsters gsAfterPlayerAI), 0.0)
                  else ((monsters gsAfterPlayerAI), newTimer)
          else ((monsters gsAfterPlayerAI), 0.0)
        
        -- 6. Kiem tra quai vat / nguoi choi chet
        survivingMonsters = filter (\m -> mPos m `notElem` flamePositions) evolvedMonsters
        playersAfterHits = updatePlayers (players gsAfterPlayerAI) flamePositions survivingMonsters
        newPlayers = updatePlayerTimers dt playersAfterHits
        livingPlayers = filter alive newPlayers
        
        -- 7. Kiem tra trang thai game
        newStatus = checkGameStatus livingPlayers survivingMonsters

        gs' = gs { board    = newBoard, 
                   players  = newPlayers,
                   bombs    = bombsAfterExplosion,
                   flames   = allFlames, 
                   powerups = allPowerUps,
                   status   = newStatus,
                   monsters = survivingMonsters,
                   monsterMoveTimer = newMonsterTimer,   
                   gamePhaseTimer = newPhaseTimer,
                   playerAIMoveTimer = newPlayerAITimer
                 }
      in
        (gs', newRng')

-- createPowerUps
createPowerUps :: StdGen -> [(Int, Int)] -> ([PowerUp], StdGen)
createPowerUps rng [] = ([], rng)
createPowerUps rng (pos:xs) =
  let
    -- 1. Quyết định xem có rơi vật phẩm không
    (roll, rng') = randomR (0.0, 1.0) rng
    
    -- 2. NÂNG CẤP: Quyết định loại vật phẩm
    -- Tăng tổng số "vé" lên 8 (thay vì 4)
    (pTypeIdx, rng'') = randomR (0 :: Int, 7 :: Int) rng' 
    
    (remainingPowerUps, finalRng) = createPowerUps rng'' xs
    
    thisPowerUp = if roll < powerUpSpawnChance
                  then let 
                         pType = case pTypeIdx of
                                     0 -> BombUp   
                                     1 -> FlameUp 
                                     2 -> Shield   
                                     3 -> BombUp  
                                     4 -> FlameUp 
                                     5 -> Shield
                                     _ -> Chaos    --Chaos chiếm 1/7 (tỉ lệ nhỏ hơn)
                       in Just (PowerUp pos pType)
                  else Nothing
  in
    case thisPowerUp of
      Just p  -> (p : remainingPowerUps, finalRng)
      Nothing -> (remainingPowerUps, finalRng)

-- updatePlayers
updatePlayers :: [Player] -> [(Int, Int)] -> [Monster] -> [Player]
updatePlayers ps flamePositions monsterPositions =
  map updatePlayer ps
  where
    monsterPos = map mPos monsterPositions
    isHit p = alive p && (pos p `elem` flamePositions || pos p `elem` monsterPos)
    
    updatePlayer p
      | isHit p =
          if iframes p > 0 || chaosTimer p > 0
          then p
          else
            if hasShield p
            then p { hasShield = False, iframes = invincibilityDuration }
            else p { alive = False }
      | otherwise = p

-- updateBoard
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
{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Types
import Data.List (mapAccumL)
import Data.Maybe (isJust, fromMaybe) -- Thêm fromMaybe

-- Cấu hình chung
bombDefaultTimer :: Float
bombDefaultTimer = 1.0

flameDuration :: Float
flameDuration = 0.5

-- HÀM MỚI: Quy định tầm nổ của bom
blastRadius :: Int
blastRadius = 2 -- Lửa sẽ lan 2 ô theo mỗi hướng

-- Kiểm tra ô có hợp lệ không (Giữ nguyên)
canMove :: Board -> (Int, Int) -> Bool
canMove b (x, y)
  | x < 0 || y < 0 = False
  | y >= length b || x >= length (head b) = False
  | otherwise =
      case (b !! y) !! x of
        Wall -> False
        Box  -> False
        _    -> True

-- Di chuyển player (Giữ nguyên)
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


-- Đặt bom (Giữ nguyên)
dropBomb :: Int -> GameState -> GameState
dropBomb pid gs =
  case [p | p@(Player i _ a) <- players gs, i == pid, a] of
    [] -> gs
    (Player _ (x, y) _ : _) ->
      gs { bombs = Bomb (x, y) bombDefaultTimer : bombs gs }

-- HÀM MỚI: Lấy ô an toàn, tránh lỗi out-of-bounds
safeGetCell :: Board -> (Int, Int) -> Maybe Cell
safeGetCell b (x, y)
  | y < 0 || y >= length b = Nothing
  | x < 0 || x >= length (head b) = Nothing
  | otherwise = Just ((b !! y) !! x)

-- HÀM MỚI: Tính toán lửa lan theo 1 hướng
getFlamesInDir :: Board -> (Int, Int) -> (Int, Int) -> Int -> [Flame]
getFlamesInDir board (x, y) (dx, dy) n
  | n <= 0 = [] -- Hết tầm nổ
  | otherwise =
      let newPos = (x + dx, y + dy)
      in case safeGetCell board newPos of
           Nothing    -> [] -- Ra ngoài bản đồ
           Just Wall  -> [] -- Đụng tường, dừng lại
           Just Box   -> [Flame newPos flameDuration] -- Đụng hòm, nổ hòm và dừng
           Just Empty -> (Flame newPos flameDuration) : getFlamesInDir board newPos (dx, dy) (n - 1)

-- HÀM MỚI: Tính toán toàn bộ vụ nổ (4 hướng + trung tâm)
calculateExplosion :: Board -> Bomb -> [Flame]
calculateExplosion board (Bomb pos _) =
  let
    (x, y) = pos
    center = [Flame pos flameDuration]
    up     = getFlamesInDir board pos ( 0, -1) blastRadius
    down   = getFlamesInDir board pos ( 0,  1) blastRadius
    left   = getFlamesInDir board pos (-1,  0) blastRadius
    right  = getFlamesInDir board pos ( 1,  0) blastRadius
  in
    center ++ up ++ down ++ left ++ right

-- Cập nhật game theo thời gian (NÂNG CẤP)
tickGame :: Float -> GameState -> GameState
tickGame dt gs@GameState{..} =
  let
    -- 1. Cập nhật timers của bom và lửa
    bombs'  = [ b { timer = timer b - dt } | b <- bombs, timer b - dt > 0 ]
    flames' = [ f { remain = remain f - dt } | f <- flames, remain f - dt > 0 ]

    -- 2. Lấy danh sách các quả bom vừa nổ
    explodingBombs = [ b | b <- bombs, timer b - dt <= 0 ]
    
    -- 3. TẠO LỬA MỚI (Đã nâng cấp)
    -- Dùng concatMap để tính toán vụ nổ cho mỗi quả bom và gộp tất cả lửa lại
    newFlames = concatMap (calculateExplosion board) explodingBombs
    
    -- 4. Gộp lửa cũ và lửa mới
    allFlames = flames' ++ newFlames
    flamePositions = [fpos f | f <- allFlames]

    -- 5. Cập nhật bảng (phá hòm)
    newBoard = updateBoard board flamePositions
    
    -- 6. Cập nhật người chơi (xử lý chết)
    newPlayers = updatePlayers players flamePositions

  in
    gs { board = newBoard, players = newPlayers, bombs = bombs', flames = allFlames }


-- HÀM CŨ (Giữ nguyên)
updatePlayers :: [Player] -> [(Int, Int)] -> [Player]
updatePlayers ps flamePositions =
  map updatePlayer ps
  where
    updatePlayer p
      | pos p `elem` flamePositions && alive p = p { alive = False }
      | otherwise = p

-- HÀM CŨ (Giữ nguyên)
updateBoard :: Board -> [(Int, Int)] -> Board
updateBoard b flamePositions =
  snd $ mapAccumL (\y row -> (y+1, snd $ mapAccumL (updateCell y) x row)) 0 b
  where
    x = 0
    updateCell y xIndex cell
      | (xIndex, y) `elem` flamePositions =
          case cell of
            Box  -> (xIndex + 1, Empty)
            _    -> (xIndex + 1, cell)
      | otherwise = (xIndex + 1, cell)
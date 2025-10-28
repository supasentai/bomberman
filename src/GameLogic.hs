module GameLogic where

import Types
import Data.List (delete, nub)

------------------------------------------------------------
-- 1️⃣ Di chuyển người chơi
------------------------------------------------------------
-- Hàm movePlayer: nhận một người chơi + action → trả về player mới
movePlayer :: Player -> Action -> Player
movePlayer p action =
  let (x, y) = pos p
  in case action of
      MoveUp    -> p { pos = (x, y - 1) }
      MoveDown  -> p { pos = (x, y + 1) }
      MoveLeft  -> p { pos = (x - 1, y) }
      MoveRight -> p { pos = (x + 1, y) }
      _         -> p  -- Idle hoặc PlaceBomb không di chuyển

------------------------------------------------------------
-- 2️⃣ Đặt bom
------------------------------------------------------------
-- Khi người chơi chọn đặt bom → tạo một Bomb mới tại vị trí của player
placeBomb :: Player -> [Bomb] -> [Bomb]
placeBomb p bs =
  let newBomb = Bomb { owner = pid p, bPos = pos p, timer = 3, power = 2 }
  in nub (newBomb : bs)  -- tránh trùng bom tại cùng vị trí

------------------------------------------------------------
-- 3️⃣ Cập nhật bom theo thời gian
------------------------------------------------------------
-- Giảm timer cho từng quả bom mỗi tick
tickBombs :: [Bomb] -> [Bomb]
tickBombs = map (\b -> b { timer = timer b - 1 })

-- Trả về danh sách bom đã nổ (timer == 0)
explodedBombs :: [Bomb] -> [Bomb]
explodedBombs = filter (\b -> timer b <= 0)

------------------------------------------------------------
-- 4️⃣ Xử lý vụ nổ
------------------------------------------------------------
-- Lửa lan theo power (tạm thời chỉ 4 hướng)
explodeBomb :: Bomb -> [Cell] -> [(Int, Int)]
explodeBomb b _ =
  let (x, y) = bPos b
      p = power b
  in [(x + dx, y) | dx <- [-p..p]] ++ [(x, y + dy) | dy <- [-p..p]]

------------------------------------------------------------
-- 5️⃣ Cập nhật GameState mỗi tick
------------------------------------------------------------
updateGame :: GameState -> GameState
updateGame gs =
  let bsTicked = tickBombs (bombs gs)
      expBombs = explodedBombs bsTicked
      flames   = concatMap (`explodeBomb` concat (board gs)) expBombs
      -- loại bỏ bom đã nổ
      remainingBombs = filter (\b -> timer b > 0) bsTicked
  in gs { bombs = remainingBombs
        , tick  = tick gs + 1
        }

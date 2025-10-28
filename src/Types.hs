module Types where

-- | Cấu trúc dữ liệu chính cho game Bomberman
-- | Được dùng chung giữa Server, Client, Render, AI,...

------------------------------------------------------------
-- 1️⃣ Các ô trên bản đồ (Cell)
------------------------------------------------------------
data Cell
  = Wall        -- Tường không phá được
  | SoftBlock   -- Khối có thể bị bom phá
  | Empty       -- Ô trống
  | Flame       -- Lửa (bom nổ)
  deriving (Eq, Show)

type Board = [[Cell]]  -- Ma trận 2D của bản đồ

------------------------------------------------------------
-- 2️⃣ Kiểu người chơi
------------------------------------------------------------
data PlayerType
  = Human       -- Người chơi thật
  | AI          -- Máy
  deriving (Eq, Show)

------------------------------------------------------------
-- 3️⃣ Người chơi (Player)
------------------------------------------------------------
data Player = Player
  { pid    :: Int           -- ID người chơi
  , pos    :: (Int, Int)    -- Vị trí hiện tại (x, y)
  , pType  :: PlayerType    -- Loại (Human / AI)
  , hp     :: Int           -- Máu (có thể mở rộng)
  } deriving (Eq, Show)

------------------------------------------------------------
-- 4️⃣ Bom
------------------------------------------------------------
data Bomb = Bomb
  { owner  :: Int           -- ID người đặt bom
  , bPos   :: (Int, Int)    -- Vị trí bom
  , timer  :: Int           -- Thời gian đếm ngược
  , power  :: Int           -- Sức công phá (bán kính nổ)
  } deriving (Eq, Show)

------------------------------------------------------------
-- 5️⃣ Trạng thái trò chơi
------------------------------------------------------------
data GameState = GameState
  { board   :: Board        -- Bản đồ hiện tại
  , players :: [Player]     -- Danh sách người chơi
  , bombs   :: [Bomb]       -- Danh sách bom đang tồn tại
  , tick    :: Int          -- Bộ đếm thời gian (tăng dần mỗi frame)
  } deriving (Eq, Show)

------------------------------------------------------------
-- 6️⃣ Hành động của người chơi
------------------------------------------------------------
data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | PlaceBomb
  | Idle
  deriving (Eq, Show)

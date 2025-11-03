{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Các ô trên bản đồ
data Cell = Empty | Wall | Box
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Board = [[Cell]]

-- | MỚI: Các loại vật phẩm
data PowerUpType
  = BombUp   -- Tăng số lượng bom
  | FlameUp  -- Tăng tầm nổ
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | MỚI: Dữ liệu vật phẩm
data PowerUp = PowerUp
  { pupPos  :: (Int, Int)
  , pupType :: PowerUpType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | NÂNG CẤP: Player giờ có chỉ số
data Player = Player
  { playerId    :: Int
  , pos         :: (Int, Int)
  , alive       :: Bool
  , maxBombs    :: Int -- Số bom tối đa
  , blastRadius :: Int -- Tầm nổ
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | NÂNG CẤP: Bom giờ "nhớ" tầm nổ và người đặt
data Bomb = Bomb
  { bpos    :: (Int, Int)
  , timer   :: Float
  , bRadius :: Int -- Tầm nổ của quả bom này
  , bOwner  :: Int -- ID của người chơi đã đặt
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Lửa (Giữ nguyên)
data Flame = Flame
  { fpos    :: (Int, Int)
  , remain  :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | NÂNG CẤP: GameState có thêm 'powerups'
data GameState = GameState
  { board    :: Board
  , players  :: [Player]
  , bombs    :: [Bomb]
  , flames   :: [Flame]
  , powerups :: [PowerUp] -- Danh sách vật phẩm
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
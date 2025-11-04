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
  = BombUp
  | FlameUp
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
  , maxBombs    :: Int
  , blastRadius :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | NÂNG CẤP: Bom
data Bomb = Bomb
  { bpos    :: (Int, Int)
  , timer   :: Float
  , bRadius :: Int
  , bOwner  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Lửa
data Flame = Flame
  { fpos    :: (Int, Int)
  , remain  :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | MỚI: Trạng thái ván đấu
data GameStatus
  = Playing
  | GameOver Int  -- Int là ID người thắng
  | Draw
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | NÂNG CẤP: GameState có thêm 'powerups' và 'status'
data GameState = GameState
  { board    :: Board
  , players  :: [Player]
  , bombs    :: [Bomb]
  , flames   :: [Flame]
  , powerups :: [PowerUp]
  , status   :: GameStatus -- MỚI: Trạng thái game
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
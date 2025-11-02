{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Các ô trên bản đồ
data Cell = Empty | Wall | Box
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Board = [[Cell]]

-- | Người chơi
data Player = Player
  { playerId :: Int
  , pos :: (Int, Int)
  , alive :: Bool
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Bom
data Bomb = Bomb
  { bpos  :: (Int, Int)
  , timer :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Lửa
data Flame = Flame
  { fpos    :: (Int, Int)
  , remain  :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Trạng thái toàn game
data GameState = GameState
  { board   :: Board
  , players :: [Player]
  , bombs   :: [Bomb]
  , flames  :: [Flame]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Cell (Giữ nguyên)
data Cell = Empty | Wall | Box
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Board = [[Cell]]

-- NÂNG CẤP: Thêm `Shield` và `Chaos`
data PowerUpType
  = BombUp
  | FlameUp
  | Shield  -- MỚI: Chặn 1 lần sát thương
  | Chaos   -- MỚI: Bất tử, nổ nhanh, xuyên bom
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- PowerUp (Giữ nguyên)
data PowerUp = PowerUp
  { pupPos  :: (Int, Int)
  , pupType :: PowerUpType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- NÂNG CẤP: Player có thêm `hasShield` và `chaosTimer`
data Player = Player
  { playerId    :: Int
  , pos         :: (Int, Int)
  , alive       :: Bool
  , maxBombs    :: Int
  , blastRadius :: Int
  , hasShield   :: Bool  -- MỚI
  , chaosTimer  :: Float -- MỚI
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Bomb (Giữ nguyên)
data Bomb = Bomb
  { bpos    :: (Int, Int)
  , timer   :: Float
  , bRadius :: Int
  , bOwner  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Flame (Giữ nguyên)
data Flame = Flame
  { fpos    :: (Int, Int)
  , remain  :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- GameStatus (Giữ nguyên)
data GameStatus
  = Playing
  | GameOver Int
  | Draw
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Monster (Giữ nguyên)
data MonsterType = Grunt | Ghost
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Monster = Monster
  { mId   :: Int
  , mPos  :: (Int, Int)
  , mType :: MonsterType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- GameState (Giữ nguyên)
data GameState = GameState
  { board            :: Board
  , players          :: [Player]
  , bombs            :: [Bomb]
  , flames           :: [Flame]
  , powerups         :: [PowerUp]
  , status           :: GameStatus
  , chatHistory      :: [String]
  , monsters         :: [Monster]
  , monsterMoveTimer :: Float
  , gamePhaseTimer   :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
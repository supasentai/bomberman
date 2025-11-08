{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Cell (Giữ nguyên)
data Cell = Empty | Wall | Box
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Board = [[Cell]]

-- PowerUpType (Giữ nguyên)
data PowerUpType
  = BombUp
  | FlameUp
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- PowerUp (Giữ nguyên)
data PowerUp = PowerUp
  { pupPos  :: (Int, Int)
  , pupType :: PowerUpType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Player (Giữ nguyên)
data Player = Player
  { playerId    :: Int
  , pos         :: (Int, Int)
  , alive       :: Bool
  , maxBombs    :: Int
  , blastRadius :: Int
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

-- MỚI: Loại quái vật
data MonsterType = Grunt | Ghost
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- NÂNG CẤP: Quái vật giờ có 'mType'
data Monster = Monster
  { mId   :: Int
  , mPos  :: (Int, Int)
  , mType :: MonsterType -- MỚI: Grunt (thường) hay Ghost (ma)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- NÂNG CẤP: GameState có thêm 'gamePhaseTimer'
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
  , gamePhaseTimer   :: Float -- MỚI: Đếm ngược 30 giây
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
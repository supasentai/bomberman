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
  | Shield
  | Chaos
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- PowerUp (Giữ nguyên)
data PowerUp = PowerUp
  { pupPos  :: (Int, Int)
  , pupType :: PowerUpType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- NÂNG CẤP: Player (9 trường)
data Player = Player
  { playerId    :: Int
  , pos         :: (Int, Int)
  , alive       :: Bool
  , maxBombs    :: Int
  , blastRadius :: Int
  , hasShield   :: Bool
  , chaosTimer  :: Float
  , iframes     :: Float -- MỚI: Khung thời gian bất tử
  , isAI        :: Bool  -- MỚI: Dành cho chế độ 1v1
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
  | Lobby
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Monster (Giữ nguyên)
data MonsterType = Grunt | Ghost
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Monster = Monster
  { mId   :: Int
  , mPos  :: (Int, Int)
  , mType :: MonsterType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- GameState (NÂNG CẤP: 11 trường)
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
  , playerAIMoveTimer  :: Float 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PlayerStatus
  = PNormal
  | PRadius Int
  | PShield
  | PChaos Float
  | PIframe Float
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

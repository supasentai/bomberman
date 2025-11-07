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

-- Monster (Giữ nguyên)
data Monster = Monster
  { mId  :: Int
  , mPos :: (Int, Int)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- NÂNG CẤP: GameState có thêm 'monsterMoveTimer'
data GameState = GameState
  { board       :: Board
  , players     :: [Player]
  , bombs       :: [Bomb]
  , flames      :: [Flame]
  , powerups    :: [PowerUp]
  , status      :: GameStatus
  , chatHistory :: [String]
  , monsters    :: [Monster]
  , monsterMoveTimer :: Float -- MỚI: Thêm timer cho quái vật
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
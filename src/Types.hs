{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Graphics.Gloss (Picture)

-- Cell
data Cell = Empty | Wall | Box
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Board = [[Cell]]

-- PowerUpType
data PowerUpType
  = BombUp
  | FlameUp
  | Shield
  | Chaos
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- PowerUp
data PowerUp = PowerUp
  { pupPos  :: (Int, Int)
  , pupType :: PowerUpType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Player = Player
  { playerId    :: Int
  , pos         :: (Int, Int)
  , alive       :: Bool
  , maxBombs    :: Int
  , blastRadius :: Int
  , hasShield   :: Bool
  , chaosTimer  :: Float
  , iframes     :: Float 
  , isAI        :: Bool 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Bomb
data Bomb = Bomb
  { bpos    :: (Int, Int)
  , timer   :: Float
  , bRadius :: Int
  , bOwner  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Flame
data Flame = Flame
  { fpos    :: (Int, Int)
  , remain  :: Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- GameStatus
data GameStatus
  = Playing
  | GameOver Int
  | Draw
  | Lobby
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Monster
data MonsterType = Grunt | Ghost
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Monster = Monster
  { mId   :: Int
  , mPos  :: (Int, Int)
  , mType :: MonsterType
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- GameState
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

data GameAssets = GameAssets
  { pWall     :: Picture
  , pBox      :: Picture
  , pEmpty    :: Picture
  , pPlayer1  :: Picture
  , pPlayer2  :: Picture
  , pBomb     :: Picture
  , pFlame    :: Picture
  , pMonster  :: Picture
  , pGhost    :: Picture
  , pBombUp   :: Picture
  , pFlameUp  :: Picture
  , pShield   :: Picture
  , pBrokenShield :: Picture
  , pChaos    :: Picture
  }
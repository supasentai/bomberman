{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Player = Player
  { pid  :: Int
  , pos  :: (Int, Int)
  , alive :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

data GameState = GameState
  { players :: [Player]
  , bombs   :: [(Int, Int)]
  } deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState

data Command = Command
  { playerId :: Int
  , action   :: String
  } deriving (Show, Generic)

instance ToJSON Command
instance FromJSON Command

-- Initial state
initGameState :: GameState
initGameState = GameState
  { players = [ Player 1 (1,1) True
              , Player 2 (5,5) True
              ]
  , bombs   = []
  }

module Types where

data Cell
  = Wall
  | SoftBlock
  | Empty
  | Flame
  deriving (Eq, Show)

type Board = [[Cell]]

data Player = Player
  { pid :: Int
  , pos :: (Int, Int)
  , alive :: Bool
  } deriving (Eq, Show)

data Bomb = Bomb
  { owner :: Int
  , bPos  :: (Int, Int)
  , timer :: Float
  } deriving (Eq, Show)

data GameState = GameState
  { board   :: Board
  , player  :: Player
  , bombs   :: [Bomb]
  , tick    :: Float
  } deriving (Eq, Show)

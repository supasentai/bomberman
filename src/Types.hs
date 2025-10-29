module Types where

------------------------------------------------------------
-- Cấu trúc dữ liệu chính
------------------------------------------------------------
data Cell
  = Wall
  | SoftBlock
  | Empty
  | Flame
  deriving (Eq, Show)

type Board = [[Cell]]

data PlayerType = Human | AI
  deriving (Eq, Show)

data Player = Player
  { pid   :: Int
  , pos   :: (Int, Int)
  , pType :: PlayerType
  , hp    :: Int
  } deriving (Eq, Show)

data Bomb = Bomb
  { owner :: Int
  , bPos  :: (Int, Int)
  , timer :: Int
  , power :: Int
  } deriving (Eq, Show)

data GameState = GameState
  { board   :: Board
  , players :: [Player]
  , bombs   :: [Bomb]
  , tick    :: Int
  } deriving (Eq, Show)

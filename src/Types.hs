{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

------------------------------------------------------------
-- | Ô trong bản đồ (Map Cell)
------------------------------------------------------------
data Cell
  = Wall        -- tường cứng
  | SoftBlock   -- tường mềm phá được
  | Empty       -- ô trống
  | Flame       -- ô có lửa (sau khi bom nổ)
  deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell

------------------------------------------------------------
-- | Người chơi
------------------------------------------------------------
data Player = Player
  { pid   :: Int          -- ID người chơi
  , pos   :: (Int, Int)   -- vị trí (x, y)
  , alive :: Bool         -- còn sống?
  } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

------------------------------------------------------------
-- | Bom
------------------------------------------------------------
data Bomb = Bomb
  { owner :: Int          -- ID người đặt bom
  , bPos  :: (Int, Int)   -- vị trí (x, y)
  , timer :: Float        -- thời gian nổ (giây)
  } deriving (Show, Eq, Generic)

instance ToJSON Bomb
instance FromJSON Bomb

------------------------------------------------------------
-- | Trạng thái tổng của game
------------------------------------------------------------
data GameState = GameState
  { board  :: Board       -- bản đồ
  , player :: Player      -- người chơi (sau này có thể là [Player])
  , bombs  :: [Bomb]      -- danh sách bom đang có
  , tick   :: Float       -- thời gian hiện tại
  } deriving (Show, Eq, Generic)

instance ToJSON GameState
instance FromJSON GameState

------------------------------------------------------------
-- | Kiểu phụ trợ
------------------------------------------------------------
type Board = [[Cell]]

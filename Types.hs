{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Types where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Set as S

-- Định nghĩa các Category (Giữ nguyên)
data Category
  = Aces | Twos | Threes | Fours | Fives | Sixes
  | ThreeKind | FourKind | FullHouse
  | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, A.FromJSON, A.ToJSON, A.FromJSONKey, A.ToJSONKey)

-- Danh sách tất cả Category (Giận nguyên)
allCategories :: [Category]
allCategories = [Aces .. Chance]

type PlayerID = Int
type Dice = [Int]
type ScoreCard = M.Map Category (Maybe Int) -- SỬA LỖI 1: Dùng 'ScoreCard'

data Mode = Human | Machine deriving (Eq, Show, Read, Generic, A.ToJSON, A.FromJSON)

-- SỬA LỖI LOGIC: Thống nhất 'Phase' (Giữ nguyên)
data Phase
  = Idle
  | Rolling
      { rollsLeft :: Int
      , held :: S.Set Int
      }
  deriving (Show, Eq, Generic, A.FromJSON, A.ToJSON)

-- Trạng thái phòng game (Giữ nguyên)
data RoomState = RoomState
  { players :: V.Vector (PlayerID, Text)
  , dice    :: Dice
  , cards   :: M.Map PlayerID ScoreCard -- SỬA LỖI 1: Dùng 'ScoreCard'
  , current :: PlayerID
  , phase   :: Phase
  } deriving (Show, Eq, Generic, A.FromJSON, A.ToJSON)

-- SỬA LỖI: Xóa 'emptyCard' khỏi đây (đã chuyển sang Core.hs)

-- BIẾN BỊ THIẾU: Đây là biến mà 'Main.hs' cần (Giữ nguyên)
initialRoomState :: RoomState
initialRoomState = RoomState
  { players = V.empty
  , dice    = [1, 1, 1, 1, 1]
  , cards   = M.empty
  , current = 0
  , phase   = Idle
  }
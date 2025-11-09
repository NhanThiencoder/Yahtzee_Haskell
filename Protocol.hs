{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Net.Protocol where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Set as S -- SỬA LỖI 2: Thêm import S.Set
import Data.Map.Strict (Map)

-- SỬA LỖI 3: Import 'PlayerID', 'ScoreCard', v.v.
import Common.Types (Category, Mode, RoomState, PlayerID, ScoreCard)


-- Client -> Server (Giữ nguyên)
data C2S
  = Join { name :: Text, mode :: Mode }
  | Start
  | Roll
  | Hold { indices :: [Int] } -- Client gửi [Int] (list)
  | Score { category :: Category }
  | Chat { msg :: Text }
  | ShowState
  | Quit
  deriving (Show, Generic, ToJSON, FromJSON)


-- Server -> Client
data S2C
  = Joined PlayerID Int -- SỬA LỖI 3: Dùng PlayerID
  | Lobby { lobbyPlayers :: [Text] }
  | State { state :: RoomState }
  -- SỬA LỖI 2: Đồng bộ 'Prompt' với 'Phase' (dùng S.Set)
  | Prompt { yourTurn :: Bool, rollsLeft :: Int, held :: S.Set Int }
  | Info { text :: Text }
  | ErrorMsg { err :: Text }
  | ChatMsg Text Text -- Thêm ChatMsg
  | End { finalScores :: [(Text, Int)] }
  deriving (Show, Generic, ToJSON, FromJSON)
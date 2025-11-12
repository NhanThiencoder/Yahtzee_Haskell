{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Net.Protocol where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Set as S
import Data.Map.Strict (Map)

import Common.Types (Category, Mode, RoomState, PlayerID, ScoreCard)


-- Client -> Server
data C2S
  = Join { name :: Text, mode :: Mode }
  | Start
  | Roll
  | Hold { indices :: [Int] }
  | Score { category :: Category }
  | Chat { msg :: Text }
  | Reset -- (THAY THẾ) Đã thay 'ShowState' bằng 'Reset'
  | Quit
  deriving (Show, Generic, ToJSON, FromJSON)


-- Server -> Client
data S2C
  = Joined PlayerID Int
  | Lobby { lobbyPlayers :: [Text] }
  | State { state :: RoomState }
  | Prompt { yourTurn :: Bool, rollsLeft :: Int, held :: S.Set Int }
  | Info { text :: Text }
  | ErrorMsg { err :: Text }
  | ChatMsg Text Text
  | End { finalScores :: [(Text, Int)] }
  deriving (Show, Generic, ToJSON, FromJSON)
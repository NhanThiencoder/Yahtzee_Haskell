module Game.Core
  ( scoreCategory
  , upperBonus
  , cardTotal
  , allFilled
  , emptyCard 
  ) where

import qualified Data.Map.Strict as M
import Data.List (group, sort, nub)
import Common.Types -- Sẽ import 'ScoreCard' và 'Category'

-- Tính điểm theo đúng rule Yahtzee
scoreCategory :: Category -> Dice -> Int
scoreCategory cat ds =
  case cat of
    Aces          -> sumOf 1
    Twos          -> sumOf 2
    Threes        -> sumOf 3
    Fours         -> sumOf 4
    Fives         -> sumOf 5
    Sixes         -> sumOf 6
    ThreeKind     -> if any (>=3) freqs then sum ds else 0
    FourKind      -> if any (>=4) freqs then sum ds else 0
    FullHouse     -> if sort (filter (>0) freqs) == [2,3] then 25 else 0
    SmallStraight -> if hasStraight 4 ds then 30 else 0
    LargeStraight -> if hasStraight 5 ds then 40 else 0
    Yahtzee       -> if any (==5) freqs then 50 else 0
    Chance        -> sum ds
  where
    sumOf x = x * length (filter (== x) ds)
    freqs   = map length . group . sort $ ds

hasStraight :: Int -> Dice -> Bool
hasStraight k ds = any (`subset` xs) windows
  where
    xs = nub (sort ds)
    windows = case k of
      4 -> [[1,2,3,4],[2,3,4,5],[3,4,5,6]]
      5 -> [[1,2,3,4,5],[2,3,4,5,6]]
      _ -> []
    subset a b = all (`elem` b) a

-- SỬA LỖI: Thêm 'emptyCard' vào đây cho logic
emptyCard :: ScoreCard
emptyCard = M.fromList [(c, Nothing) | c <- [minBound .. maxBound]]

-- SỬA LỖI 1: Đổi 'Scorecard' -> 'ScoreCard'
upperBonus :: ScoreCard -> Int
upperBonus card
  | upper >= 63 = 35
  | otherwise   = 0
  where
    val c = case M.lookup c card >>= id of
              Just v  -> v
              Nothing -> 0
    upper = sum [ val Aces, val Twos, val Threes, val Fours, val Fives, val Sixes ]

-- SỬA LỖI 1: Đổi 'Scorecard' -> 'ScoreCard'
cardTotal :: ScoreCard -> Int
cardTotal card = sum [ maybe 0 id v | (_, v) <- M.toList card ] + upperBonus card

-- SỬA LỖI 1: Đổi 'Scorecard' -> 'ScoreCard'
allFilled :: ScoreCard -> Bool
allFilled = all (/= Nothing) . M.elems
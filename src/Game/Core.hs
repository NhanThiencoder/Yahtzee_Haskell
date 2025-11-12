module Game.Core
  ( scoreCategory
  , upperBonus
  , cardTotal
  , allFilled
  , emptyCard 
  , aiChooseHold
  , aiChooseScore
  ) where

import qualified Data.Map.Strict as M
import Data.List (group, sort, nub, maximumBy, find)
import Data.Ord (comparing)
import Data.Maybe (isNothing, fromMaybe)
import Control.Applicative ((<|>))
import Common.Types

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

-- (SỬA LỖI) Di chuyển 'subset' ra ngoài và SỬA LỖI LOGIC
-- Hàm 'all' cần 2 tham số: (1) hàm (`elem` b), (2) danh sách (a)
subset :: (Eq a, Foldable t) => [a] -> t a -> Bool
subset a b = all (`elem` b) a -- <-- LỖI ĐÃ ĐƯỢC SỬA Ở ĐÂY

emptyCard :: ScoreCard
emptyCard = M.fromList [(c, Nothing) | c <- [minBound .. maxBound]]

upperBonus :: ScoreCard -> Int
upperBonus card
  | upper >= 63 = 35
  | otherwise   = 0
  where
    val c = case M.lookup c card >>= id of
              Just v  -> v
              Nothing -> 0
    upper = sum [ val Aces, val Twos, val Threes, val Fours, val Fives, val Sixes ]

cardTotal :: ScoreCard -> Int
cardTotal card = sum [ maybe 0 id v | (_, v) <- M.toList card ] + upperBonus card

allFilled :: ScoreCard -> Bool
allFilled = all (/= Nothing) . M.elems


--------------------------------------------------------------------------------
-- (THÊM MỚI) BỘ NÃO AI
--------------------------------------------------------------------------------

-- Đếm số lần xuất hiện của mỗi mặt
diceCounts :: Dice -> M.Map Int Int
diceCounts ds = M.fromListWith (+) [(d, 1) | d <- ds]

-- Tìm các index của một giá trị
indicesOfValue :: Dice -> Int -> [Int]
indicesOfValue ds val =
  [ i | (i, d) <- zip [0..] ds, d == val ]

-- AI quyết định NÊN GIỮ (HOLD) xúc xắc nào
-- Trả về danh sách các index (0-4) cần giữ
aiChooseHold :: Dice -> [Int]
aiChooseHold ds =
  let counts = diceCounts ds
      -- Tìm giá trị xuất hiện nhiều nhất
      (valMost, numMost) = M.foldlWithKey'
                            (\acc k v -> if v > snd acc then (k,v) else acc)
                            (0,0)
                            counts

      valFour = fst <$> find ((== 4) . snd) (M.assocs counts)

      -- Kiểm tra gần Small Straight
      xs = nub (sort ds)
      isAlmostSmall = any (`subset` xs) [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]

  in case valFour of
      -- 1. Ưu tiên 4 of a Kind
      Just val -> indicesOfValue ds val

      -- 2. Ưu tiên giữ Full House
      _ | sort (M.elems counts) == [2,3] -> [0..4]

      -- 3. Ưu tiên giữ Large Straight
      _ | hasStraight 5 ds -> [0..4]

      -- 4. Ưu tiên giữ Small Straight
      _ | hasStraight 4 ds -> [0..4]

      -- 5. Ưu tiên giữ 3 of a Kind
      _ | numMost == 3 -> indicesOfValue ds valMost

      -- 6. Ưu tiên giữ 1 phần của Straight
      _ | isAlmostSmall ->
          let straightPart = filter (`elem` [1..6]) xs
          in concatMap (indicesOfValue ds) straightPart

      -- 7. Mặc định: Giữ các con có số lần xuất hiện > 1
      _ | numMost > 1 -> indicesOfValue ds valMost

      -- 8. Không có gì: Không giữ gì cả (roll lại tất cả)
      _ -> []

-- AI quyết định NÊN CHỌN Ô ĐIỂM (SCORE) nào
aiChooseScore :: Dice -> ScoreCard -> Category
aiChooseScore ds card =
  let
    -- 1. Tìm tất cả các ô còn trống
    availableCats = M.keys $ M.filter isNothing card

    -- 2. Tính điểm cho TẤT CẢ các ô
    allScores = [ (cat, scoreCategory cat ds) | cat <- [minBound .. maxBound] ]

    -- 3. Lọc ra điểm của các ô CÒN TRỐNG
    availableScores = filter (\(cat, _) -> cat `elem` availableCats) allScores

    -- 4. Tìm ô có điểm cao nhất
    (bestCat, bestScore) = if null availableScores
                             then (Chance, 0)
                             else maximumBy (comparing snd) availableScores

    -- 5. Chiến lược phụ: Nếu có Yahtzee (50đ) thì luôn lấy
    yahtzeeScore = scoreCategory Yahtzee ds
    yahtzeeAvail = isNothing (card M.! Yahtzee)

  in if yahtzeeScore == 50 && yahtzeeAvail
       then Yahtzee
       else if bestScore > 0
         then bestCat
         -- Chiến lược cuối: Bỏ đi ô có điểm 0, ưu tiên bỏ phần trên
         else fromMaybe Chance $
                find (`elem` [Aces,Twos,Threes,Fours,Fives,Sixes]) availableCats
                <|> (fst <$> find (\(c,s) -> c `elem` availableCats) allScores)
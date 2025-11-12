module Client where

import Network.Socket
import System.IO
import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Char as Char
import System.Process (callCommand)
import Common.Types
import Net.Protocol
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

clearScreen :: IO ()
clearScreen = callCommand "cls"  -- dùng 'clear' nếu chạy trên Linux/Mac

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, port, name] -> run host port name
    _ -> putStrLn "Usage: yahtzee-client <host> <port> <name>"

run :: String -> String -> String -> IO ()
run host port name = withSocketsDo $ do
  addr <- resolve host port
  sock <- open addr
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering

  -- Gửi join
  hPutStrLn h (BL.unpack (A.encode (Join (T.pack name) Human)))

  -- Thread đọc server
  _ <- forkIO $ forever $ do
    msgLine <- hGetLine h
    case A.eitherDecode' (BL.pack msgLine) :: Either String S2C of
      Right msg -> renderMessage msg (T.pack name)
      Left err  -> putStrLn ("[Decode error] " ++ err ++ " on: " ++ msgLine)

  -- Thread nhập lệnh người chơi
  forever $ do
    putStr "> "
    cmd <- getLine
    let sendC2S x = hPutStrLn h (BL.unpack (A.encode x))
    case words cmd of
      ["/start"]       -> sendC2S Start
      ["/roll"]        -> sendC2S Roll
      ("/hold":xs)     -> sendC2S (Hold (map read xs))
      ["/score", c]    -> case parseCategory c of
                            Just cat -> sendC2S (Score cat)
                            Nothing  -> putStrLn "Invalid category."
      ["/reset"]       -> sendC2S Reset
      ["/quit"]        -> sendC2S Quit >> pure ()
      _                -> sendC2S (Chat (T.pack cmd))

-- Tách ra để Main.hs (TUI) có thể tái sử dụng
resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    []    -> ioError (userError ("Address not found: " ++ host ++ ":" ++ port))
    (a:_) -> pure a

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  pure sock

-- Hiển thị gọn gàng thông tin
renderMessage :: S2C -> T.Text -> IO ()
renderMessage msg myName = case msg of
  State st -> do
    clearScreen
    renderScoreboard st myName
    putStrLn ""

    let curName = currentPlayer st
    let rolls = rollsLeftPhase st

    if phase st == Idle
      then putStrLn "Game idle. Press /start when ready."
      else if curName == myName
        then putStrLn $ "YOUR TURN. Rolls left: " ++ show rolls
        else putStrLn $ "Waiting for " ++ T.unpack curName ++ "..."

    putStrLn $ "Dice: " ++ show (dice st)
    putStrLn ""

  Prompt _ _ _ ->
    pure () -- Bỏ qua, vì 'State' đã xử lý

  Info t -> putStrLn ("[Info] " ++ T.unpack t)
  ErrorMsg t -> putStrLn ("[Error] " ++ (T.unpack t))

  -- (THÊM) Hiển thị chat
  ChatMsg from t -> putStrLn $ "[" ++ T.unpack from ++ "] " ++ T.unpack t

  Joined seat totalSeats ->
    putStrLn $ "You joined. Your seat: " ++ show seat ++ "/" ++ show totalSeats

  Lobby players ->
    putStrLn $ "Players in lobby: " ++ show players

  End results -> do
    clearScreen
    putStrLn "===== GAME OVER ====="
    let m_myScore = lookup myName results
    let m_maxScore = if null results then Nothing else Just (maximum (map snd results))

    case (m_myScore, m_maxScore) of
      (Just myScore, Just maxScore) | myScore == maxScore -> putStrLn "YOU WIN!"
      _ -> putStrLn "You lose."

    mapM_ (\(n, s) -> putStrLn (T.unpack n ++ ": " ++ show s)) results

  _ -> pure ()


currentPlayer :: RoomState -> T.Text
currentPlayer st =
  let ps = players st
      curPid = current st
      -- (SỬA) Xử lý 3-tuple (pid, name, mode)
      mName = lookup curPid (map (\(a,b,_) -> (a, b)) (V.toList ps))
  -- (SỬA LỖI) Dùng T.pack "?" để khớp kiểu T.Text
  in fromMaybe (T.pack "?") mName

rollsLeftPhase :: RoomState -> Int
rollsLeftPhase st = case phase st of
  Rolling r _ -> r
  _           -> 0

parseCategory :: String -> Maybe Category
parseCategory s = case map toLower s of
  "aces" -> Just Aces
  "twos" -> Just Twos
  "threes" -> Just Threes
  "fours" -> Just Fours
  "fives" -> Just Fives
  "sixes" -> Just Sixes
  "threekind" -> Just ThreeKind
  "fourkind" -> Just FourKind
  "fullhouse" -> Just FullHouse
  "smallstraight" -> Just SmallStraight
  "largestraight" -> Just LargeStraight
  "yahtzee" -> Just Yahtzee
  "chance" -> Just Chance
  _ -> Nothing
  where toLower = toEnum . fromEnum . Char.toLower

-- (SỬA LỖI) Toàn bộ hàm này cần cập nhật cho (PlayerID, Text, Mode)
renderScoreboard :: RoomState -> T.Text -> IO ()
renderScoreboard st myName = do
  let ps = players st -- V.Vector (PlayerID, Text, Mode)
      cardMap = cards st
      cats = [minBound .. maxBound] :: [Category]

      -- (SỬA) Lấy Tên (phần tử thứ 2) từ 3-tuple
      names = map (T.unpack . (\(_,b,_) -> b)) (V.toList ps)

      getScore pid cat =
        maybe "-" (maybe "-" show) $
          M.lookup pid cardMap >>= M.lookup cat

  putStrLn $ replicate 40 '='
  putStrLn $ "Player: " ++ T.unpack myName
  putStrLn $ replicate 40 '='
  putStrLn "Yahtzee - Live Scoreboard"
  putStrLn "----------------------------------------"
  putStrLn $ "Category          | " ++ unwords (map (printf "%6s" :: String -> String) names)
  putStrLn "----------------------------------------"

  let (upperCats, lowerCats) = splitAt 6 cats

      -- (ĐÃ SỬA LOGIC BONUS)
      getScoreInt pid cat = fromMaybe 0 $ M.lookup pid cardMap >>= M.lookup cat >>= id
      sumUpper pid = sum [ getScoreInt pid cat | cat <- upperCats ]
      getBonus pid = if sumUpper pid >= 63 then 35 else 0
      getBonusDisplay pid = let s = sumUpper pid in s + getBonus pid
      sumLower pid = sum [ getScoreInt pid cat | cat <- lowerCats ]
      totalScore pid = getBonusDisplay pid + sumLower pid

      -- Hàm in hàng
      printRow cat = do
        printf "%-17s" (show cat)
        -- (SỬA) Xử lý 3-tuple (pid, name, mode)
        mapM_ (\(pid, _, _) -> printf "| %6s " (getScore pid cat)) (V.toList ps)
        putStrLn ""

      -- Hàm in hàng tính toán
      printCalcRow label calcFn = do
        printf "%-17s" label
        mapM_ (\(pid, _, _) -> printf "| %6s " (show (calcFn pid))) (V.toList ps)
        putStrLn ""

  -- In bảng
  mapM_ printRow upperCats
  putStrLn "----------------------------------------"
  printCalcRow "Bonus" getBonusDisplay
  putStrLn "----------------------------------------"
  mapM_ printRow lowerCats
  putStrLn "----------------------------------------"
  printCalcRow "Total" totalScore
  putStrLn "----------------------------------------"
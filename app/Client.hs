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
      Right msg -> renderMessage msg name
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
      ["/show"]        -> sendC2S ShowState
      ["/quit"]        -> sendC2S Quit >> pure () -- TODO: Thoát
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
renderMessage :: S2C -> String -> IO ()
renderMessage msg playerName = case msg of
  State st -> do
    clearScreen
    renderScoreboard st playerName
    putStrLn ""
    if currentPlayer st == playerName
      then putStrLn "It's your turn!"
      else putStrLn $ "Waiting for " ++ currentPlayer st ++ "..."
    putStrLn $ "Dice: " ++ show (dice st)
    putStrLn $ "Rolls left: " ++ show (rollsLeftPhase st)
    putStrLn ""
  
  -- SỬA LỖI ĐỒNG BỘ 1:
  Prompt yourTurn rolls _ ->
    putStrLn $ "[Turn] Your turn: " ++ show yourTurn ++ ". Rolls left: " ++ show rolls

  Info t -> putStrLn ("[Info] " ++ T.unpack t)
  ErrorMsg t -> putStrLn ("[Error] " ++ (T.unpack t))

  -- SỬA LỖI ĐỒNG BỘ 2:
  Joined seat totalSeats ->
    putStrLn $ "You joined. Your seat: " ++ show seat ++ "/" ++ show totalSeats
  
  Lobby players ->
    putStrLn $ "Players in lobby: " ++ show players

  End results -> do
    clearScreen
    putStrLn "===== GAME OVER ====="
    mapM_ (\(n, s) -> putStrLn (T.unpack n ++ ": " ++ show s)) results
  _ -> pure ()


currentPlayer :: RoomState -> String
currentPlayer st =
  let ps = players st
  in if V.null ps
       then "-"
       else T.unpack (snd (ps V.! current st))

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

renderScoreboard :: RoomState -> String -> IO ()
renderScoreboard st playerName = do
  let ps = players st
      scoreMap = cards st
      -- SỬA LỖI ĐỒNG BỘ 3 (Tối ưu):
      cats = [minBound .. maxBound] :: [Category]
      names = map (T.unpack . snd) (V.toList ps)
      getScore pid cat =
        maybe "-" (maybe "-" show) $
          M.lookup pid scoreMap >>= M.lookup cat
  putStrLn $ replicate 40 '='
  putStrLn $ "Player: " ++ playerName
  putStrLn $ replicate 40 '='
  putStrLn "Yahtzee - Live Scoreboard"
  putStrLn "----------------------------------------"
  putStrLn $ "Category          | " ++ unwords (map (printf "%6s" :: String -> String) names)
  putStrLn "----------------------------------------"
  mapM_ (\cat -> do
          printf "%-17s" (show cat)
          mapM_ (\(pid, _) -> printf "| %6s " (getScore pid cat)) (V.toList ps)
          putStrLn "") cats
  putStrLn "----------------------------------------"
  -- TODO: Tính tổng (logic trong Game.Core.cardTotal)
  putStrLn "Total             | ..."
  putStrLn "----------------------------------------"
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.UI.Threepenny.Core hiding (text)
import qualified Graphics.UI.Threepenny as UI

import           Network.Socket              (socketToHandle)
import           System.IO
import           System.Environment          (getArgs)
import           Control.Concurrent          (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad               (forever, void, forM, when, unless)
import           Data.List                   (find)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Maybe                  (fromMaybe)

import           Common.Types                ( RoomState(..)
                                             , Category(..)
                                             , Mode(..)
                                             , Phase(..)
                                             , allCategories
                                             )
import           Net.Protocol
import           Client                      (resolve, open)
import           Game.Core                   (scoreCategory)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      [host, port, nameStr] ->
        runClient host port nameStr 8023
      [host, port, nameStr, guiPortStr] ->
        runClient host port nameStr (read guiPortStr)
      _ ->
        putStrLn "Usage: yahtzee-gui <host> <port> <player-name> [gui-port]"

runClient :: String -> String -> String -> Int -> IO ()
runClient host port nameStr guiPort = do
    addr <- resolve host port
    sock <- open addr
    h    <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering

    let playerName = T.pack nameStr
    hPutStrLn h (BL.unpack (A.encode (Join playerName Human)))

    chan <- newChan
    _ <- forkIO $ forever $ do
      line <- hGetLine h
      case A.eitherDecode' (BL.pack line) :: Either String S2C of
        Right msg -> writeChan chan msg
        Left err  -> writeChan chan (ErrorMsg (T.pack ("Decode error: " ++ err)))

    let config = UI.defaultConfig
          { UI.jsPort = Just guiPort
          }
    startGUI config (setup h chan playerName)

    hClose h

--------------------------------------------------------------------------------
-- GUI
--------------------------------------------------------------------------------

setup :: Handle -> Chan S2C -> T.Text -> Window -> UI ()
setup h chan _playerName win = do
    return win # set UI.title "Yahtzee (Threepenny)"

    -- === TẠO CÁC KHỐI GIAO DIỆN ===

    -- (Cột Trái) Bảng điểm
    scoreHolder <- UI.div  #. "scoreHolder"

    -- (Cột Phải - Phần 1) Bảng điều khiển
    infoLabel   <- UI.span #. "info"
    btnStart <- UI.button #+ [string "Start"]
    btnRoll  <- UI.button #+ [string "Roll"]
    btnReset <- UI.button #+ [string "Reset"]
    btnQuit  <- UI.button #+ [string "Quit"]
    diceElems <- mapM (const mkDie) [0..4 :: Int]

    controls <- UI.div #. "controls" #+
      [ UI.element btnStart
      , UI.element btnRoll
      , UI.element btnReset
      , UI.element btnQuit
      , UI.br
      , UI.br
      , UI.element infoLabel
      ]
    diceRow <- UI.tr #+
      [ UI.td # set style [("padding", "0 10px")] #+ [UI.element die]
      | die <- diceElems
      ]
    controlRow <- UI.tr #+
      [ UI.td # set UI.colspan 5 #+ [UI.element controls] ]
    gamePanel <- UI.table #. "gamePanel" #+
      [ UI.element diceRow
      , UI.element controlRow
      ]

    -- (Cột Phải - Phần 2) LỊCH SỬ CHAT
    chatLog <- UI.div #. "chatLog"
      # set style [ ("height", "300px")
                  , ("overflow-y", "scroll")
                  , ("border", "1px solid #ccc")
                  , ("padding", "5px")
                  , ("margin-top", "10px")
                  , ("background-color", "#f9f9f9")
                  ]

    -- (Cột Phải - Phần 3) Ô NHẬP LIỆU CHAT
    chatInput <- UI.input
      # set (UI.attr "placeholder") "Type message and press Enter..."
      # set style [("width", "calc(100% - 60px)")]
    btnSend <- UI.button #+ [string "Send"]
      # set style [("width", "50px"), ("margin-left", "5px")]
    chatInputArea <- UI.div # set style [("margin-top", "10px")] #+
      [ element chatInput, element btnSend ]

    -- === GHÉP BỐ CỤC 2 CỘT ===

    leftColumn <- UI.div #+ [element scoreHolder]
    -- (SỬA BỐ CỤC) Đã sửa lại thứ tự: gamePanel -> chatLog -> chatInputArea
    rightColumn <- UI.div #+ [element gamePanel, element chatLog, element chatInputArea]

    root <- UI.table #. "rootTable" #+
      [ UI.tr #+
          [ UI.td # set UI.valign "top" #+ [element leftColumn]
          , UI.td # set UI.valign "top" #+ [element rightColumn]
          ]
      ]

    getBody win #+ [UI.element root]

    -- === XỬ LÝ SỰ KIỆN ===

    let sendC2S msg =
          liftIO $ hPutStrLn h (BL.unpack (A.encode msg))

    let sendChatAction = do
          val <- get value chatInput
          unless (null val) $ do
            sendC2S (Chat (T.pack val))
            void $ element chatInput # set value "" -- (SỬA LỖI) Thêm 'void'

    on UI.click btnSend $ const sendChatAction
    on UI.keyup chatInput $ \keycode ->
      when (keycode == 13) sendChatAction

    on UI.click btnStart $ const $ sendC2S Start
    on UI.click btnRoll $ const $ sendC2S Roll
    on UI.click btnReset $ const $ sendC2S Reset

    on UI.click btnQuit $ const $ do
      sendC2S Quit
      liftIO $ hClose h
      UI.runFunction $ ffi "window.location.reload()"

    sequence_
      [ on UI.click die $ const (sendC2S (Hold [i]))
      | (i, die) <- zip [0..] diceElems
      ]

    -- Thread lắng nghe server
    liftIO . forkIO . forever $ do
      msg <- readChan chan
      -- (SỬA LỖI) Sửa lỗi gõ nhầm 'sendC2G S'
      runUI win $ handleServerMessage sendC2S scoreHolder diceElems infoLabel chatLog _playerName msg

    return ()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

showDie :: Int -> String
showDie n = case n of
  1 -> "⚀"
  2 -> "⚁"
  3 -> "⚂"
  4  -> "⚃"
  5 -> "⚄"
  6 -> "⚅"
  _ -> "·"

mkDie :: UI Element
mkDie =
  UI.span #. "die"
          # set style [ ("font-size","32px")
                      , ("cursor", "pointer")
                      , ("padding", "2px 6px")
                      , ("border-radius", "4px")
                      , ("background-color", "#fff")
                      ]
          #+ [string (showDie 0)]

currentPlayerName :: RoomState -> T.Text
currentPlayerName st =
  let pidCur = current st
  -- (SỬA LỖI) Xử lý 3-tuple (pid, name, mode)
  in case find (\(p,_,_) -> p == pidCur) (V.toList (players st)) of
       Just (_, name, _) -> name
       Nothing           -> T.pack "?"

rollsLeftPhase :: RoomState -> Int
rollsLeftPhase st = case phase st of
  Rolling r _ -> r
  _           -> 0

handleServerMessage
  :: (C2S -> UI ())
  -> Element  -- scoreHolder
  -> [Element] -- diceElems
  -> Element  -- infoLabel
  -> Element  -- chatLog
  -> T.Text   -- myName
  -> S2C
  -> UI ()
handleServerMessage sendC2S scoreHolder diceElems infoLabel chatLog myName msg =

  let setInfo :: String -> [(String, String)] -> UI ()
      setInfo s extraStyle = void $
        element infoLabel
          # set UI.text s
          # set style (("font-size", "1.1em") : ("margin-top", "15px") : ("display", "block") : extraStyle)

  in case msg of
    State st -> do
      table <- mkScoreTable sendC2S st myName
      void $ element scoreHolder # set children [table]

      let heldSet = case phase st of
                      Rolling _ h -> h
                      _           -> S.empty

      updateDice diceElems (dice st) heldSet

      let curName = currentPlayerName st
      let rolls = rollsLeftPhase st

      if phase st == Idle
        then setInfo "Game idle. Press Start when ready." [("font-weight", "normal"), ("color", "black")]
        else if curName == myName
          then setInfo ("YOUR TURN. Rolls left: " ++ show rolls) [("font-weight", "bold"), ("color", "green")]
          else setInfo ("Waiting for " ++ T.unpack curName ++ "...") [("font-weight", "normal"), ("color", "black")]

    Prompt _ _ _ -> do
      pure ()

    Joined pid total ->
      setInfo ("Joined as seat " ++ show pid ++ " / " ++ show total) [("font-weight", "bold"), ("color", "#007bff")]

    Lobby ps ->
      setInfo ("Lobby: " ++ show (map T.unpack ps)) [("font-weight", "normal"), ("color", "black")]

    Info t ->
      setInfo (T.unpack t) [("font-weight", "bold"), ("color", "#007bff")]

    ErrorMsg t ->
      setInfo ("Error: " ++ T.unpack t) [("font-weight", "bold"), ("color", "red")]

    ChatMsg from txt -> do
      newMsg <- UI.div #. "chatMessage" #+
        [ UI.span # set style [("font-weight", "bold")] #+ [string (T.unpack from ++ ": ")]
        , UI.span #+ [string (T.unpack txt)]
        ]
      element chatLog #+ [element newMsg]
      -- (SỬA LỖI) Xóa 'UI.element'
      UI.runFunction $ ffi "var el = %1; el.scrollTop = el.scrollHeight;" chatLog


    End results -> do
      let scores = map snd results
      let maxScore = if null scores then 0 else maximum scores
      let myScore = lookup myName results
      let (winMsg, winStyle) = case myScore of
            Just score | score == maxScore ->
                if length (filter (== maxScore) scores) > 1
                then ("HÒA!", [("color", "blue")])
                else ("BẠN THẮNG!", [("color", "green")])
            Just _  ->
                ("BẠN THUA!", [("color", "red")])
            Nothing ->
                ("Game Over", [])

      let resultsStr = "Game over: " ++ renderResults results

      void $ element infoLabel # set children []
      void $ element infoLabel #+
        [ UI.span # set style (("font-weight", "bold") : ("font-size", "1.3em") : winStyle)
                  #+ [string winMsg]
        , UI.br
        , UI.span #+ [string resultsStr]
        ]

renderResults :: [(T.Text, Int)] -> String
renderResults xs =
  unwords [ T.unpack n ++ "=" ++ show s | (n,s) <- xs ]

mkScoreTable :: (C2S -> UI ()) -> RoomState -> T.Text -> UI Element
mkScoreTable sendC2S st myName =
  let ps      = players st
      cardMap = cards st

      getScoreInt :: Int -> Category -> Int
      getScoreInt pid cat =
        fromMaybe 0 $ M.lookup pid cardMap >>= M.lookup cat >>= id

      (upperCats, lowerCats) = splitAt 6 allCategories

      sumUpper :: Int -> Int
      sumUpper pid = sum [ getScoreInt pid cat | cat <- upperCats ]

      -- (ĐÃ SỬA LOGIC BONUS)
      -- getBonus (dùng để tính điểm thưởng 0 hoặc 35)
      getBonus :: Int -> Int
      getBonus pid = if sumUpper pid >= 63 then 35 else 0

      -- getBonusDisplay (dùng để HIỂN THỊ ở hàng Bonus)
      -- = (Tổng trên) + (Điểm thưởng)
      getBonusDisplay :: Int -> Int
      getBonusDisplay pid =
        let s = sumUpper pid
        in s + getBonus pid -- Sửa logic: Luôn cộng (s + 0) hoặc (s + 35)

      sumLower :: Int -> Int
      sumLower pid = sum [ getScoreInt pid cat | cat <- lowerCats ]

      -- totalScore (dùng để HIỂN THỊ ở hàng Total)
      -- = (Giá trị hàng Bonus) + (Tổng dưới)
      totalScore :: Int -> Int
      totalScore pid = getBonusDisplay pid + sumLower pid

      header :: UI Element
      header =
        UI.tr #+
          ( UI.th # set UI.align "left" #+ [string "Category"]
          -- (SỬA LỖI) Xử lý 3-tuple (pid, name, mode)
          : [ (if pName == myName
                 then UI.th # set style [("background-color", "#ffe6e6")]
                 else UI.th
              )
              # set UI.align "right"
              # set UI.width 60
              #+ [string (T.unpack pName)]
            | (_pid, pName, _) <- V.toList ps
            ]
          )

      mkCategoryRow :: Category -> UI Element
      mkCategoryRow cat = do
        catButton <- UI.button #. "categoryButton" #+ [string (show cat)]
        on UI.click catButton $ const $ sendC2S (Score cat)

        -- (SỬA LỖI) Xử lý 3-tuple (pid, name, mode)
        scoreCellElements <- forM (V.toList ps) $ \(pid, _, _) -> do
          let mScore = M.lookup pid cardMap >>= M.lookup cat

          case mScore of
            Just (Just n) ->
              return $ UI.td # set UI.align "right"
                             # set style [("font-weight", "bold")]
                             #+ [string (show n)]

            _ -> do
              let rolls = rollsLeftPhase st
              let isCurrentTurn = (pid == current st)
              -- (SỬA LỖI) Chỉ hiển thị gợi ý khi ĐÃ ROLL
              let showHints = isCurrentTurn && (phase st /= Idle) && (rolls < 3)

              if showHints
                then do
                  let hintScore = scoreCategory cat (dice st)
                  return $ UI.td # set UI.align "right"
                                 # set style [("color", "#aaa")]
                                 #+ [string (show hintScore)]
                else do
                  return $ UI.td # set UI.align "right"
                                 #+ [string "-"]

        UI.tr #+ (UI.td # set UI.align "left" #+ [element catButton] : scoreCellElements)


      mkDisplayRow :: String -> (Int -> Int) -> UI Element
      mkDisplayRow label calcFn = do
        let boldStyle = [("font-weight", "bold")]

        -- (SỬA LỖI) Xử lý 3-tuple (pid, name, mode)
        let scoreCells = map (\(pid, _, _) ->
                            let score = calcFn pid
                            in UI.td # set UI.align "right" # set style boldStyle #+ [string (show score)]
                           ) (V.toList ps)

        UI.tr #+ (UI.td # set UI.align "left" # set style boldStyle #+ [string label] : scoreCells)

  in do
    let upperRows = map mkCategoryRow upperCats
    let bonusRow  = mkDisplayRow "Bonus" getBonusDisplay
    let lowerRows = map mkCategoryRow lowerCats
    let totalRow  = mkDisplayRow "Total" totalScore

    let allRows = upperRows ++ [bonusRow] ++ lowerRows ++ [totalRow]

    UI.table #. "scoreTable"
             # set UI.border 1
             # set UI.cellpadding 5
             # set UI.cellspacing 0
             #+ (header : allRows)


updateDice :: [Element] -> [Int] -> S.Set Int -> UI ()
updateDice diceElems vals heldSet = do
  let padded = take (length diceElems) (vals ++ repeat 0)
  sequence_
    [ do
        element el # set UI.text (showDie v)
        let isHeld = i `S.member` heldSet
        let commonStyle = [ ("font-size","32px")
                          , ("cursor", "pointer")
                          , ("padding", "2px 6px")
                          , ("border-radius", "4px")
                          ]
        -- (SỬA LỖI) Logic màu Hold/Reset
        let bgStyle = if isHeld
                        then ("background-color", "#ddd") -- Xám nếu Giữ
                        else ("background-color", "#fff") -- Trắng nếu không Giữ
        element el # set style (bgStyle : commonStyle)

    | (i, (el, v)) <- zip [0..] (zip diceElems padded)
    ]
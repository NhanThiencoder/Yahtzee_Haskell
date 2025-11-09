{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.UI.Threepenny.Core hiding (text)
import qualified Graphics.UI.Threepenny as UI

import           Network.Socket              (socketToHandle)
import           System.IO
import           System.Environment          (getArgs)
import           Control.Concurrent          (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad               (forever, void, forM)
import           Data.List                   (find)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S -- (THÊM) Import Data.Set để xử lý 'held'

import           Common.Types                ( RoomState(..)
                                             , Category(..)
                                             , Mode(..)
                                             , Phase(..) -- (THÊM) Import Phase
                                             , allCategories
                                             )
import           Net.Protocol
import           Client                      (resolve, open)

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

    startGUI UI.defaultConfig { UI.jsPort = Just guiPort }
             (setup h chan playerName)

    hClose h

--------------------------------------------------------------------------------
-- GUI
--------------------------------------------------------------------------------

setup :: Handle -> Chan S2C -> T.Text -> Window -> UI ()
setup h chan _playerName win = do
    return win # set UI.title "Yahtzee (Threepenny)"

    scoreHolder <- UI.div  #. "scoreHolder"
    infoLabel   <- UI.span #. "info"

    btnStart <- UI.button #+ [string "Start"]
    btnRoll  <- UI.button #+ [string "Roll"]
    btnShow  <- UI.button #+ [string "Show"]
    btnQuit  <- UI.button #+ [string "Quit"]

    diceElems <- mapM (const mkDie) [0..4 :: Int]

    controls <- UI.div #. "controls" #+
      [ UI.element btnStart
      , UI.element btnRoll
      , UI.element btnShow
      , UI.element btnQuit
      , UI.br
      , UI.br
      , UI.element infoLabel
      ]

    -- hàng xúc xắc + hàng nút
    diceRow <- UI.tr #+
      [ UI.td #+ [UI.element die] | die <- diceElems ]

    controlRow <- UI.tr #+
      [ UI.td #+ [UI.element controls] ]

    gamePanel <- UI.table #. "gamePanel" #+
      [ UI.element diceRow
      , UI.element controlRow
      ]

    root <- UI.table #. "rootTable" #+
      [ UI.tr #+
          [ UI.td #+ [UI.element scoreHolder]
          , UI.td #+ [UI.element gamePanel]
          ]
      ]

    getBody win #+ [UI.element root]

    let sendC2S msg =
          liftIO $ hPutStrLn h (BL.unpack (A.encode msg))

    on UI.click btnStart $ const $ do
      sendC2S Start
      sendC2S ShowState

    on UI.click btnRoll $ const $ do
      sendC2S Roll

    on UI.click btnShow $ const $ do
      sendC2S ShowState

    on UI.click btnQuit $ const $ do
      sendC2S Quit
      liftIO $ hClose h -- (Nên đóng handle khi quit)
      -- Tải lại trang để ngắt kết nối
      UI.runFunction $ ffi "window.location.reload()"

    -- (THAY ĐỔI) Thêm sự kiện click cho các nút xúc xắc
    sequence_
      [ on UI.click die $ const (sendC2S (Hold [i]))
      | (i, die) <- zip [0..] diceElems
      ]

    liftIO . forkIO . forever $ do
      msg <- readChan chan
      runUI win $ handleServerMessage sendC2S scoreHolder diceElems infoLabel msg

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
          # set style [("font-size","32px"),("margin","12px"), ("cursor", "pointer")]
          #+ [string (showDie 0)]

handleServerMessage
  :: (C2S -> UI ())
  -> Element
  -> [Element]
  -> Element
  -> S2C
  -> UI ()
handleServerMessage sendC2S scoreHolder diceElems infoLabel msg =
  case msg of
    State st -> do
      table <- mkScoreTable sendC2S st
      void $ element scoreHolder # set children [table]

      let heldSet = case phase st of
                      Rolling _ h -> h
                      _           -> S.empty

      updateDice diceElems (dice st) heldSet

      let curName = T.unpack (currentPlayerName st)
      void $ element infoLabel # set UI.text ("Turn: " ++ curName)

    Prompt yourTurn rolls _held -> do
      let prefix = if yourTurn then "Your turn. " else "Waiting. "
      void $ element infoLabel
             # set UI.text (prefix ++ "Rolls left: " ++ show rolls)

    Joined pid total ->
      void $ element infoLabel
             # set UI.text ("Joined as seat " ++ show pid ++ " / " ++ show total)

    Lobby ps ->
      void $ element infoLabel
             # set UI.text ("Lobby: " ++ show (map T.unpack ps))

    Info t ->
      void $ element infoLabel # set UI.text (T.unpack t)

    ErrorMsg t ->
      void $ element infoLabel
             # set UI.text ("Error: " ++ T.unpack t)

    ChatMsg from txt ->
      void $ element infoLabel
             # set UI.text (T.unpack from ++ ": " ++ T.unpack txt)

    End results ->
      void $ element infoLabel
             # set UI.text ("Game over: " ++ renderResults results)

renderResults :: [(T.Text, Int)] -> String
renderResults xs =
  unwords [ T.unpack n ++ "=" ++ show s | (n,s) <- xs ]

mkScoreTable :: (C2S -> UI ()) -> RoomState -> UI Element
mkScoreTable sendC2S st =
  let ps      = players st
      names   = [ T.unpack name | (_, name) <- V.toList ps ]
      cardMap = cards st

      getScore pid cat =
        case M.lookup pid cardMap >>= M.lookup cat of
          Just (Just n) -> show n
          _             -> ""

      header :: UI Element
      header =
        UI.tr #+
          ( UI.th #+ [string "Category"]
          : [ UI.th #+ [string n] | n <- names ]
          )

      -- Chữ ký kiểu này (UI [UI Element]) là chính xác
      rows :: UI [UI Element]
      rows = forM allCategories $ \cat -> do
        catButton <- UI.button #. "categoryButton" #+ [string (show cat)]
        
        on UI.click catButton $ const $ sendC2S (Score cat)

        -- `scoreCells` có kiểu `[UI Element]`
        scoreCells <- forM (V.toList ps) $ \(pid, _) ->
          return $ UI.td #+ [string (getScore pid cat)]

        -- (SỬA LỖI 1) Xóa `map element` khỏi `scoreCells`
        -- `scoreCells` đã là kiểu `[UI Element]`
        return $ UI.tr #+ (UI.td #+ [element catButton] : scoreCells)

  in do
    -- `rowElems` có kiểu `[UI Element]`
    rowElems <- rows

    -- (SỬA LỖI 2) Xóa `map element` khỏi `rowElems`
    -- `rowElems` đã là kiểu `[UI Element]`
    UI.table #. "scoreTable" #+ (header : rowElems)


updateDice :: [Element] -> [Int] -> S.Set Int -> UI ()
updateDice diceElems vals heldSet = do
  let padded = take (length diceElems) (vals ++ repeat 0)
  sequence_
    [ do
        element el # set UI.text (showDie v)

        let isHeld = i `S.member` heldSet
        let baseStyle = [ ("font-size","32px")
                        , ("margin","12px")
                        , ("cursor", "pointer")
                        , ("padding", "2px 6px") 
                        , ("border-radius", "4px") 
                        ]
        let heldStyle = ("background-color", "#ddd")

        if isHeld
          then element el # set style (heldStyle : baseStyle)
          else element el # set style baseStyle

    | (i, (el, v)) <- zip [0..] (zip diceElems padded)
    ]

currentPlayerName :: RoomState -> T.Text
currentPlayerName st =
  let pidCur = current st
  in case find ((== pidCur) . fst) (V.toList (players st)) of
       Just (_, name) -> name
       Nothing        -> "?"
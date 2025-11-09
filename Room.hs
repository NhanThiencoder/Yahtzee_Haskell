module Game.Room
  ( Room(..)
  , Event(..)
  , newRoom
  , serveEvent
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, when, void)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

-- Import các module thư viện của bạn
import Common.Types
import Net.Protocol hiding (rollsLeft, held)
import Game.Core (scoreCategory, cardTotal, allFilled, emptyCard)

-- ROOM với event-loop (Giữ nguyên)
data Room = Room
  { inbox :: TChan Event
  , outbox :: TChan S2C
  , stVar :: TVar RoomState
  }

-- Các sự kiện từ Server (Giữ nguyên)
data Event
  = EJoin PlayerID T.Text
  | EStart PlayerID
  | ERoll PlayerID
  | EHold PlayerID [Int] -- Client gửi một danh sách (thường là 1 phần tử)
  | EScore PlayerID Category
  | EShow PlayerID
  | EQuit PlayerID

-- Khởi tạo phòng (Giữ nguyên)
newRoom :: IO Room
newRoom = atomically $ do
  ib <- newTChan
  ob <- newBroadcastTChan
  st <- newTVar RoomState
        { players = V.empty
        , current = 0
        , phase = Idle
        , dice = replicate 5 1
        , cards = M.empty
        }
  pure Room { inbox = ib, outbox = ob, stVar = st }

-- Lăn xúc xắc
rollDice :: S.Set Int -> [Int] -> IO [Int]
rollDice heldIdx ds = do
  let keep i = i `S.member` heldIdx
  forM (zip [0..] ds) $ \(i, d) ->
    if keep i then pure d else randomRIO (1,6)

-- Gửi cho mọi client (Giữ nguyên)
broadcastSTM :: Room -> S2C -> STM ()
broadcastSTM Room{outbox} msg = writeTChan outbox msg

-- (SỬA LỖI 2) Helper: Chuyển lượt và RESET 'held' về S.empty
advanceTurn :: RoomState -> RoomState
advanceTurn st =
  let ps = players st
      n = V.length ps
  in if n == 0
       then st { phase = Idle }
       else
         let curPID = current st
             mCurIdx = V.findIndex ((== curPID) . fst) ps
             -- Tìm index của người chơi tiếp theo
             nextIdx = (fromMaybe 0 mCurIdx + 1) `mod` n
             nextPID = fst (ps V.! nextIdx)
         in st { current = nextPID
               -- Đây là logic reset: set rolls = 3 và held = S.empty
               , phase = Rolling 3 S.empty
               , dice = replicate 5 1
               }

-- Xử lý Event chính
serveEvent :: Room -> Event -> IO ()
serveEvent room@Room{stVar} ev = do
  -- Xử lý IO (chỉ Lăn xúc xắc)
  (m_ds, m_err) <- case ev of
    ERoll pid -> handleRoll pid
    _         -> pure (Nothing, Nothing)
  
  -- Xử lý STM (logic chính)
  atomically $ do
    st <- readTVar stVar
    case m_err of
      Just err -> broadcastSTM room (ErrorMsg err)
      Nothing -> do
        -- Cập nhật state với xúc xắc mới (nếu có)
        let st' = case m_ds of
                    Nothing -> st
                    Just ds ->
                      let (Rolling r h) = phase st
                      in st { dice = ds, phase = Rolling (r - 1) h }
        -- Xử lý event
        handleEventSTM room st' ev

  where
    -- Xử lý IO (Roll)
    handleRoll pid = do
      (m_roll, m_err) <- atomically $ do
        st <- readTVar stVar
        if pid /= current st
          then pure (Nothing, Just "Not your turn.")
          else case phase st of
                 Rolling r h | r > 0 -> pure (Just (r, h, dice st), Nothing)
                 _ -> pure (Nothing, Just "Cannot roll now.")
      
      case m_roll of
        Nothing -> pure (Nothing, m_err)
        Just (r, h, ds) -> do
          ds' <- rollDice h ds -- 'h' là S.Set Int
          pure (Just ds', Nothing)

    -- Xử lý STM
    handleEventSTM :: Room -> RoomState -> Event -> STM ()
    handleEventSTM room st ev = case ev of
      EJoin pid nm -> do
        let already = any ((== pid) . fst) (V.toList (players st))
        when (phase st == Idle && not already) $ do
          let players' = V.snoc (players st) (pid, nm)
              cards' = M.insert pid emptyCard (cards st)
              st' = st { players = players', cards = cards' }
          writeTVar stVar st'
          broadcastSTM room (Joined pid (V.length players'))
          broadcastSTM room (Info $ nm <> " joined.")
          broadcastSTM room (State st')

      EStart pid -> do
        let n = V.length (players st)
        if n < 2 && phase st == Idle
          then broadcastSTM room (ErrorMsg "Need at least 2 players.")
          else when (phase st == Idle) $ do
            let firstPlayerPID = fst (players st V.! 0)
                st' = st { current = firstPlayerPID, phase = Rolling 3 S.empty }
            writeTVar stVar st'
            broadcastSTM room (State st')
            broadcastSTM room (Info "Game started! Rolling phase begins.")

      ERoll pid -> do
        -- State đã được cập nhật bởi 'm_ds' ở bên ngoài
        writeTVar stVar st
        broadcastSTM room (State st)

      -- (SỬA LỖI 1) Logic "Toggle" (lật trạng thái)
      EHold pid hs -> do
        when (pid == current st) $ do
          case phase st of
            Rolling r oldHeld -> do
              -- Hàm helper: lật trạng thái của 1 index 'h'
              let toggle h s = if h `S.member` s then S.delete h s else S.insert h s
              
              -- Áp dụng 'toggle' cho mọi index client gửi (thường là 1)
              let finalHeld = foldr toggle oldHeld (filter (\i -> i >= 0 && i < 5) hs)
              
              let st' = st { phase = Rolling r finalHeld }
              writeTVar stVar st'
              broadcastSTM room (State st')
            _ -> broadcastSTM room (ErrorMsg "Cannot hold now")

      EScore pid cat -> do
        when (pid == current st) $ do
          case phase st of
            Rolling _ _ -> do
              let card = M.findWithDefault emptyCard pid (cards st)
              case M.lookup cat card of
                Just Nothing -> do -- Chỉ cho phép ghi vào ô trống
                  let pts = scoreCategory cat (dice st)
                      card' = M.insert cat (Just pts) card
                      cards' = M.insert pid card' (cards st)
                      st' = st { cards = cards' }
                  
                  let allCards = M.elems cards'
                      finished = all allFilled allCards
                  
                  if finished
                    then do
                      -- Game kết thúc
                      let results = [ (nm, cardTotal (M.findWithDefault emptyCard p cards'))
                                    | (p, nm) <- V.toList (players st') ]
                      let stEnd = st' { phase = Idle }
                      writeTVar stVar stEnd
                      broadcastSTM room (State stEnd)
                      broadcastSTM room (End results)
                    else do
                      -- Game tiếp tục, chuyển lượt (và reset hold)
                      let stNext = advanceTurn st'
                      writeTVar stVar stNext
                      broadcastSTM room (State stNext)
                _ -> broadcastSTM room (ErrorMsg "Category already filled")
            _ -> broadcastSTM room (ErrorMsg "Cannot score now")

      EShow pid -> do
        broadcastSTM room (State st)

      EQuit pid -> do
        let m_idx = V.findIndex ((== pid) . fst) (players st)
        case m_idx of
          Nothing -> pure ()
          Just idx -> do
            let (pid, nm) = players st V.! idx
            let players' = V.filter ((/= pid) . fst) (players st)
                cards' = M.delete pid (cards st)
                n' = V.length players'
                st' = st { players = players', cards = cards' }
            
            broadcastSTM room (Info $ nm <> " left the game.")

            if phase st == Idle
              then do
                writeTVar stVar st'
                broadcastSTM room (State st')
              else do
                if n' < 2
                  then do
                    -- Không đủ người chơi, reset game
                    let stEnd = st' { phase = Idle, current = 0, dice = replicate 5 1 }
                    writeTVar stVar stEnd
                    broadcastSTM room (State stEnd)
                    broadcastSTM room (Info "Game reset. Not enough players.")
                  else do
                    -- Nếu người thoát là người đang chơi, chuyển lượt
                    if pid == current st
                      then do
                        -- Dùng 'current = pid' để 'advanceTurn' tìm đúng người tiếp theo
                        let stNext = advanceTurn st' { current = pid } 
                        writeTVar stVar stNext
                        broadcastSTM room (State stNext)
                      else do
                        -- Người khác thoát, giữ nguyên lượt
                        writeTVar stVar st'
                        broadcastSTM room (State st')
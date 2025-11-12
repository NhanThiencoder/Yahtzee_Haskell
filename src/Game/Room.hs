module Game.Room
  ( Room(..)
  , Event(..)
  , newRoom
  , serveEvent
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad (forM, when, void)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import System.Random (randomRIO)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S

import Common.Types
import Net.Protocol hiding (rollsLeft, held)
import Game.Core (scoreCategory, cardTotal, allFilled, emptyCard, aiChooseHold, aiChooseScore)

-- ROOM
data Room = Room
  { inbox :: TChan Event
  , outbox :: TChan S2C
  , stVar :: TVar RoomState
  }

-- (THAY ĐỔI) Cập nhật EJoin, thêm EAI_TakeTurn
data Event
  = EJoin PlayerID T.Text Mode
  | EStart PlayerID
  | ERoll PlayerID
  | EHold PlayerID [Int]
  | EScore PlayerID Category
  | EReset PlayerID
  | EQuit PlayerID
  | EAI_TakeTurn PlayerID -- (THÊM MỚI) Event nội bộ để kích hoạt AI

-- Khởi tạo phòng
newRoom :: IO Room
newRoom = atomically $ do
  ib <- newTChan
  ob <- newBroadcastTChan
  st <- newTVar initialRoomState
  pure Room { inbox = ib, outbox = ob, stVar = st }

-- Lăn xúc xắc
rollDice :: S.Set Int -> [Int] -> IO [Int]
rollDice heldIdx ds = do
  let keep i = i `S.member` heldIdx
  forM (zip [0..] ds) $ \(i, d) ->
    if keep i then pure d else randomRIO (1,6)

-- Gửi cho mọi client
broadcastSTM :: Room -> S2C -> STM ()
broadcastSTM Room{outbox} msg = writeTChan outbox msg

-- (THAY ĐỔI) Cập nhật 'advanceTurn' để nó hoạt động với (PID, Name, Mode)
advanceTurn :: RoomState -> RoomState
advanceTurn st =
  let ps = players st
      n = V.length ps
  in if n == 0
       then st { phase = Idle }
       else
         let curPID = current st
             mCurIdx = V.findIndex (\(p,_,_) -> p == curPID) ps -- (SỬA)
             -- Tìm index của người chơi tiếp theo
             nextIdx = (fromMaybe 0 mCurIdx + 1) `mod` n
             nextPID = (\(p,_,_) -> p) (ps V.! nextIdx) -- (SỬA)
         in st { current = nextPID
               , phase = Rolling 3 S.empty
               , dice = replicate 5 1
               }

-- (THÊM MỚI) Hàm chạy lượt AI (chạy trong 1 thread riêng)
runAITurn :: Room -> PlayerID -> IO ()
runAITurn room@Room{stVar, inbox} aiPid = do
  let sendAIEvent ev = atomically $ writeTChan inbox ev

  -- 1. Đợi 1 giây (cho cảm giác thật)
  threadDelay (1 * 1000000)

  -- 2. Lượt Roll 1
  sendAIEvent (ERoll aiPid)
  threadDelay (2 * 1000000) -- Đợi 2s (cho người chơi thấy xúc xắc)

  -- 3. Quyết định Giữ Lần 1
  st1 <- readTVarIO stVar
  let dice1 = dice st1
  let holds1 = aiChooseHold dice1

  -- Chỉ Hold/Roll nếu còn lượt
  when (rollsLeftPhase st1 > 0 && not (null holds1)) $ do
    sendAIEvent (EHold aiPid holds1)
    threadDelay (1 * 1000000)

    -- 4. Lượt Roll 2
    sendAIEvent (ERoll aiPid)
    threadDelay (2 * 1000000)

    -- 5. Quyết định Giữ Lần 2
    st2 <- readTVarIO stVar
    let dice2 = dice st2
    let holds2 = aiChooseHold dice2

    when (rollsLeftPhase st2 > 0 && not (null holds2)) $ do
      sendAIEvent (EHold aiPid holds2)
      threadDelay (1 * 1000000)

      -- 6. Lượt Roll 3 (lượt cuối)
      sendAIEvent (ERoll aiPid)
      threadDelay (2 * 1000000)

  -- 7. Quyết định Chọn Điểm
  stFinal <- readTVarIO stVar
  let finalDice = dice stFinal
  let aiCard = fromMaybe emptyCard (M.lookup aiPid (cards stFinal))
  let catToScore = aiChooseScore finalDice aiCard

  threadDelay (1 * 1000000)
  sendAIEvent (EScore aiPid catToScore)

  where
    rollsLeftPhase st = case phase st of
      Rolling r _ -> r
      _           -> 0


-- Xử lý Event chính
serveEvent :: Room -> Event -> IO ()
serveEvent room@Room{stVar, inbox} ev = do
  case ev of
    EAI_TakeTurn pid -> do
      void $ forkIO $ runAITurn room pid
      pure ()
    _ -> do
      (m_ds, m_err) <- case ev of
        ERoll pid -> handleRoll pid
        _         -> pure (Nothing, Nothing)

      m_aiPidToRun <- atomically $ do
        st <- readTVar stVar
        case m_err of
          Just err -> broadcastSTM room (ErrorMsg err) >> pure Nothing
          Nothing -> do
            let st' = case m_ds of
                        Nothing -> st
                        Just ds ->
                          let (Rolling r h) = phase st
                          in st { dice = ds, phase = Rolling (r - 1) h }
            handleEventSTM room st' ev

      case m_aiPidToRun of
        Just aiPid -> atomically $ writeTChan inbox (EAI_TakeTurn aiPid)
        Nothing -> pure ()

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
          ds' <- rollDice h ds
          pure (Just ds', Nothing)

    -- (THAY ĐỔI) Hàm này giờ trả về 'STM (Maybe PlayerID)'
    handleEventSTM :: Room -> RoomState -> Event -> STM (Maybe PlayerID)
    handleEventSTM room st ev = case ev of

      EJoin pid nm m -> do
        let already = isJust $ V.find (\(p,_,_) -> p == pid) (players st)
        when (phase st == Idle && not already) $ do
          let players' = V.snoc (players st) (pid, nm, m)
              cards' = M.insert pid emptyCard (cards st)
              st' = st { players = players', cards = cards' }
          writeTVar stVar st'
          broadcastSTM room (Joined pid (V.length players'))
          broadcastSTM room (Info $ nm <> " joined.")
          broadcastSTM room (State st')
        pure Nothing

      EStart pid -> do
        let n = V.length (players st)
        let m_player = V.find (\(p,_,_) -> p == pid) (players st)

        case (n, m_player) of
          (1, Just (_, _, Human)) -> do
            let aiPid = -1
            let aiName = "Bot AI"
            let players' = V.snoc (players st) (aiPid, aiName, Machine)
            let newCards = M.fromList [ (p, emptyCard) | (p, _, _) <- V.toList players' ]
            let st' = st { players = players'
                         , current = pid
                         , phase = Rolling 3 S.empty
                         , cards = newCards
                         }
            writeTVar stVar st'
            broadcastSTM room (State st')
            broadcastSTM room (Info $ "Bot AI has joined. Your turn!")
            pure Nothing

          (n, _) | n >= 2 && phase st == Idle -> do
            let (firstPlayerPID, firstPlayerName, firstPlayerMode) = (players st V.! 0)
            let newCards = M.fromList [ (p, emptyCard) | (p, _, _) <- V.toList (players st) ]
            let st' = st { current = firstPlayerPID
                         , phase = Rolling 3 S.empty
                         , cards = newCards
                         }
            writeTVar stVar st'
            broadcastSTM room (State st')
            broadcastSTM room (Info $ "Game started! " <> firstPlayerName <> "'s turn.")

            if firstPlayerMode == Machine
              then pure (Just firstPlayerPID)
              else pure Nothing

          (1, Just (_, _, Machine)) -> broadcastSTM room (ErrorMsg "Bots cannot start games.") >> pure Nothing
          _ -> broadcastSTM room (ErrorMsg "Need at least 1 human player to start.") >> pure Nothing


      ERoll pid -> do
        writeTVar stVar st
        broadcastSTM room (State st)
        pure Nothing

      EHold pid hs -> do
        when (pid == current st) $ do
          case phase st of
            Rolling r oldHeld -> do
              let toggle h s = if h `S.member` s then S.delete h s else S.insert h s
              let finalHeld = foldr toggle oldHeld (filter (\i -> i >= 0 && i < 5) hs)
              let st' = st { phase = Rolling r finalHeld }
              writeTVar stVar st'
              broadcastSTM room (State st')
            _ -> broadcastSTM room (ErrorMsg "Cannot hold now")
        pure Nothing

      -- (SỬA LỖI) Thay thế 'when' bằng 'if/then/else'
      EScore pid cat ->
        if pid == current st
          then case phase st of
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
                                    | (p, nm, _) <- V.toList (players st') ]
                      let stEnd = st' { phase = Idle }
                      writeTVar stVar stEnd
                      broadcastSTM room (State stEnd)
                      broadcastSTM room (End results)
                      pure Nothing -- Game kết thúc, không có lượt AI
                    else do
                      -- Game tiếp tục, chuyển lượt (và reset hold)
                      let stNext = advanceTurn st'
                      writeTVar stVar stNext
                      broadcastSTM room (State stNext)

                      -- Kiểm tra xem lượt tiếp theo có phải AI không
                      let (nextPid, _, nextMode) = players stNext V.! (fromMaybe 0 $ V.findIndex (\(p,_,_) -> p == current stNext) (players stNext))
                      if nextMode == Machine
                        then pure (Just nextPid)
                        else pure Nothing

                _ -> broadcastSTM room (ErrorMsg "Category already filled") >> pure Nothing
            _ -> broadcastSTM room (ErrorMsg "Cannot score now") >> pure Nothing
          else
            pure Nothing -- Không phải lượt của bạn, không kích hoạt AI

      EReset pid -> do
        let newCards = M.fromList [ (p, emptyCard) | (p, _, _) <- V.toList (players st) ]
        let st' = st { players = players st
                     , current = 0
                     , phase = Idle
                     , dice = replicate 5 1
                     , cards = newCards
                     }
        writeTVar stVar st'
        broadcastSTM room (State st')
        broadcastSTM room (Info "Game has been reset by a player!")
        pure Nothing

      EQuit pid -> do
        let m_idx = V.findIndex (\(p,_,_) -> p == pid) (players st)
        case m_idx of
          Nothing -> pure Nothing
          Just idx -> do
            let (pid, nm, _) = players st V.! idx
            let players' = V.filter (\(p,_,_) -> p /= pid) (players st)
                cards' = M.delete pid (cards st)
                n' = V.length players'
                st' = st { players = players', cards = cards' }

            broadcastSTM room (Info $ nm <> " left the game.")

            if phase st == Idle
              then do
                writeTVar stVar st'
                broadcastSTM room (State st')
                pure Nothing
              else do
                if n' < 2
                  then do
                    let stEnd = st' { phase = Idle, current = 0, dice = replicate 5 1 }
                    writeTVar stVar stEnd
                    broadcastSTM room (State stEnd)
                    broadcastSTM room (Info "Game reset. Not enough players.")
                    pure Nothing
                  else do
                    if pid == current st
                      then do
                        let stNext = advanceTurn st' { current = pid }
                        writeTVar stVar stNext
                        broadcastSTM room (State stNext)
                        -- (THAY ĐỔI) KIỂM TRA AI sau khi chuyển lượt
                        let (nextPid, _, nextMode) = players stNext V.! (fromMaybe 0 $ V.findIndex (\(p,_,_) -> p == current stNext) (players stNext))
                        if nextMode == Machine
                          then pure (Just nextPid)
                          else pure Nothing
                      else do
                        writeTVar stVar st'
                        broadcastSTM room (State st')
                        pure Nothing

      EAI_TakeTurn _ -> pure Nothing
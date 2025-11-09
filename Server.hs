{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Network.Socket hiding (openSocket)
import           System.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import           Control.Concurrent         (forkIO)
import           Control.Monad              (forever, unless, void)
import           Control.Concurrent.STM
import qualified Data.Vector                as V
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Control.Exception          (SomeException, catch)
import           Common.Types               (PlayerID, RoomState(..))
import           Game.Room                  (Room(..), Event(..), newRoom, serveEvent) -- Đã import thêm Event, newRoom, serveEvent
import           Net.Protocol

-- Resolve listening address
resolveAddr :: String -> IO AddrInfo
resolveAddr port = do
  let hints = defaultHints { addrFlags       = [AI_PASSIVE]
                           , addrSocketType = Stream
                           }
  addrs <- getAddrInfo (Just hints) Nothing (Just port)
  case addrs of
    []    -> ioError (userError ("Address not found for port: " ++ port))
    (a:_) -> pure a

-- Open listening socket (KHÔNG dùng bracket, giữ socket sống)
openSocket :: AddrInfo -> IO Socket
openSocket addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 64
  pure sock

-- === MAIN SERVER LOOP ===
runServer :: String -> IO ()
runServer port = withSocketsDo $ do
  room <- newRoom
  addr <- resolveAddr port
  sock <- openSocket addr
  putStrLn $ "Server listening on *:" ++ port

  -- Logger: đọc từ bản copy của broadcast channel
  _ <- forkIO $ do
    logChan <- atomically $ dupTChan (outbox room)
    forever $ do
      msg <- atomically $ readTChan logChan
      putStrLn $ "BROADCAST → " ++ BL.unpack (A.encode msg)

  -- === THÊM VÒNG LẶP GAME TẠI ĐÂY ===
  -- Luồng (thread) này liên tục đọc event từ `inbox`
  -- và dùng `serveEvent` để xử lý chúng.
  _ <- forkIO $ forever $ do
    event <- atomically $ readTChan (inbox room)
    serveEvent room event
  -- ===================================

  -- Accept loop
  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "New connection from: " ++ show peer
    pidMilli <- round . (* 1000) <$> getPOSIXTime
    let pid :: PlayerID
        pid = fromIntegral pidMilli
    void $ forkIO $ serveClient room conn peer pid

-- === SERVE ONE CLIENT ===
serveClient :: Room -> Socket -> SockAddr -> PlayerID -> IO ()
serveClient room sock peer pid = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering

  -- Subscribe vào broadcast
  out <- atomically $ dupTChan (outbox room)

  -- Gửi event cho Room
  let sendEvent ev = atomically $ writeTChan (inbox room) ev

  -- Giải mã C2S
  let handleC2S cmd = case cmd of
        Join nm _ -> sendEvent (EJoin pid nm)
        -- SỬA LỖI Ở DÒNG NÀY:
        Start     -> sendEvent (EStart pid)
        Roll      -> sendEvent (ERoll pid)
        Hold is   -> sendEvent (EHold pid is)
        Score c   -> sendEvent (EScore pid c)
        ShowState -> sendEvent (EShow pid)
        Quit      -> ioError (userError "Client quit")
        Chat t    -> do
          st <- readTVarIO (stVar room)
          let senderName =
                maybe "???" snd (V.find ((== pid) . fst) (players st))
          atomically $ writeTChan (outbox room) (ChatMsg senderName t)

      reader = forever $ do
        line <- hGetLine h
        unless (null line) $ do
          case A.eitherDecode' (BL.pack line) :: Either String C2S of
            Right cmd -> handleC2S cmd
            Left err  -> do
              putStrLn $ "Decode error from " ++ show peer ++ ": " ++ err
              atomically $
                writeTChan (outbox room)
                           (ErrorMsg (T.pack "Invalid command"))

      writer = forever $ do
        msg <- atomically $ readTChan out
        hPutStrLn h (BL.unpack (A.encode msg))

      cleanup (e :: SomeException) = do
        putStrLn $ "Client disconnected from " -- ++ show peer -- (peer không có trong scope, bỏ qua)
                ++ " with error: " ++ show e
        sendEvent (EQuit pid)
        hClose h

  _ <- forkIO writer
  reader `catch` cleanup

-- === MAIN ===
main :: IO ()
main = runServer "3000"
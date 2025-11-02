{-# LANGUAGE RecordWildCards #-}

module Server where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, forM)
import Control.Exception (handle, catch, IOException)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (catMaybes)
import Data.List (filter) -- Thêm import này

import Types
import GameLogic

-- Cấu hình tick rate (30 FPS, giống client)
tickRate :: Int
tickRate = 30

-- Thời gian chờ mỗi tick (tính bằng micro giây)
tickDelay :: Int
tickDelay = 1000000 `div` tickRate

-- Delta time (thời gian trôi qua mỗi tick, tính bằng giây)
dt :: Float
dt = fromIntegral tickDelay / 1000000.0

-- (Giữ nguyên initBoard và initGameState)
initBoard :: Board
initBoard =
  [ [ if x == 0 || y == 0 || x == 8 || y == 8 then Wall
      -- SỬA: Đảm bảo ô xuất phát của player trống
      else if (x,y) `elem` [(1,1), (1,2), (2,1), (7,7), (7,6), (6,7)] then Empty 
      else if (x + y) `mod` 3 == 0 then Box
      else Empty
    | x <- [0..8] ]
  | y <- [0..8]
  ]

initGameState :: GameState
initGameState = GameState initBoard
  [ Player 1 (1,1) True
  , Player 2 (7,7) True
  ]
  [] []


-- | Luồng chính của server
runServer :: IO ()
runServer = withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 2
  putStrLn "🔥 Server started at port 4242"

  stateVar   <- newTVarIO initGameState
  -- TẠO MỚI: Một biến để lưu trữ tất cả các client
  clientsVar <- newTVarIO []

  -- TẠO MỚI: Chạy game loop trong một luồng riêng
  _ <- forkIO $ gameLoop stateVar clientsVar

  -- Vòng lặp chấp nhận client mới
  forever $ do
    (conn, _) <- accept sock
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h LineBuffering
    putStrLn "✅ Client connected!"

    -- Thêm client mới vào danh sách
    atomically $ modifyTVar clientsVar (h:)

    -- Gửi trạng thái game ban đầu
    gs0 <- readTVarIO stateVar
    BL.hPutStrLn h (encode gs0)

    -- Chạy luồng xử lý riêng cho client này
    forkIO $ clientHandler h stateVar clientsVar

-- | Vòng lặp game chính (chạy độc lập)
gameLoop :: TVar GameState -> TVar [Handle] -> IO ()
gameLoop stateVar clientsVar = forever $ do
  -- 1. Chờ cho đến tick tiếp theo
  threadDelay tickDelay

  -- 2. Cập nhật trạng thái game (ví dụ: bom nổ)
  -- gs là trạng thái MỚI NHẤT sau khi tick
  gs <- atomically $ do
    currentGs <- readTVar stateVar
    let newGs = tickGame dt currentGs
    writeTVar stateVar newGs
    return newGs

  -- 3. Gửi trạng thái mới cho TẤT CẢ client
  handles <- readTVarIO clientsVar
  -- `broadcast` sẽ gửi state và tự động xóa các client đã ngắt kết nối
  newHandles <- broadcast handles gs
  atomically $ writeTVar clientsVar newHandles

-- | Gửi state cho tất cả client, trả về danh sách client còn sống
broadcast :: [Handle] -> GameState -> IO [Handle]
broadcast handles gs = do
  results <- forM handles $ \h -> do
    -- Dùng `catch` để phát hiện client đã ngắt kết nối
    catch (do
             BL.hPutStrLn h (encode gs)
             return (Just h) -- Trả về Just h nếu gửi thành công
           )
          (\e -> let _ = e :: IOException in return Nothing) -- Trả về Nothing nếu lỗi
  -- Lọc ra danh sách chỉ chứa các handle gửi thành công
  return (catMaybes results)

-- | Xử lý input từ một client
clientHandler :: Handle -> TVar GameState -> TVar [Handle] -> IO ()
clientHandler h stateVar clientsVar =
  -- `handle` sẽ bắt lỗi (ví dụ: client ngắt kết nối)
  handle (disconnectHandler h clientsVar) $
    forever $ do
      line <- hGetLine h
      -- Chỉ cần cập nhật state, gameLoop sẽ lo việc gửi đi
      atomically $ do
          gs <- readTVar stateVar
          let gs' = updateFromCommand gs line
          writeTVar stateVar gs'

-- | Xử lý khi client ngắt kết nối
disconnectHandler :: Handle -> TVar [Handle] -> IOException -> IO ()
disconnectHandler h clientsVar _ = do
  putStrLn "Client disconnected."
  -- Xóa client khỏi danh sách
  atomically $ modifyTVar clientsVar (filter (/= h))

-- SỬA LỖI LOGIC: Đảo ngược 'w' và 's'
updateFromCommand :: GameState -> String -> GameState
updateFromCommand gs cmd
    | cmd == "w" = movePlayer 1 ( 0,  1) gs
    | cmd == "s" = movePlayer 1 ( 0, -1) gs
    | cmd == "a" = movePlayer 1 (-1,  0) gs
    | cmd == "d" = movePlayer 1 ( 1,  0) gs
    | cmd == "b" = dropBomb 1 gs
    | otherwise  = gs
module Render where

import Graphics.Gloss
import Types
import Debug.Trace
import Data.List (take)

cellSize :: Float
cellSize = 40

-- SỬA LỖI: Thu nhỏ bản đồ (0.6) VÀ CĂN GIỮA
drawGame :: GameState -> Picture
drawGame gs =
  case board gs of
    [] -> Translate 0 0 $ Scale 0.2 0.2 $ Text "Board is empty"
    b@(firstRow:_) ->
      -- Dùng `Scale` bên ngoài `Translate` để căn giữa
      Scale 0.6 0.6 $ -- SỬA LỖI: Thu nhỏ bản đồ (từ 0.65 -> 0.6)
      Translate (-w'/2) (-h'/2) $
      Pictures $
        (concat
          [ [drawBoard b]
          , drawPlayers h (players gs)
          , drawBombs h (bombs gs)
          , drawFlames h (flames gs)
          , drawPowerUps h (powerups gs)
          , drawMonsters h (monsters gs)
          ])
        ++ [drawStatus (status gs) (w, h)]
      where
        w = length firstRow
        h = length b
        w' = fromIntegral w * cellSize
        h' = fromIntegral h * cellSize

-- drawBoard (Giữ nguyên)
drawBoard :: Board -> Picture
drawBoard b =
  Pictures
    [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
        (drawCell c)
    | (y, row) <- zip [0..] (reverse b)
    , (x, c) <- zip [0..] row
    ]

-- drawCell (Giữ nguyên)
drawCell :: Cell -> Picture
drawCell Wall  = color (greyN 0.2) $ rectangleSolid cellSize cellSize
drawCell Box   = color (greyN 0.4) $ rectangleSolid cellSize cellSize
drawCell Empty = color (greyN 0.7) $ rectangleSolid cellSize cellSize

-- drawPlayers (Giữ nguyên)
drawPlayers :: Int -> [Player] -> [Picture]
drawPlayers h ps =
  [ translate (fromIntegral x * cellSize)
              (fromIntegral (h - 1 - y) * cellSize)
      $ Pictures
          [ color (dark (greyN 0.1)) (translate 3 (-3) (circleSolid r))
          , color col (circleSolid r)
          ]
  | Player pid (x,y) True _ _ <- ps
  , let col = if pid == 1 then red else blue
  , let r = cellSize / 3
  ]

-- drawBombs (Giữ nguyên)
drawBombs :: Int -> [Bomb] -> [Picture]
drawBombs h bs =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ Scale s s
      $ color (dark red) (thickCircle (cellSize/3) 6)
  | Bomb (x,y) t _ _ <- bs
  , let s = 0.8 + 0.4 * (t / 3)
  ]

-- drawFlames (Giữ nguyên)
drawFlames :: Int -> [Flame] -> [Picture]
drawFlames h fs =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ color (makeColorI 255 180 0 (round (255 * alpha)))
      $ rectangleSolid (cellSize*0.9) (cellSize*0.9)
  | Flame (x,y) r <- fs
  , let alpha = max 0 (r / 1.5)
  ]

-- drawPowerUps (Giữ nguyên)
drawPowerUps :: Int -> [PowerUp] -> [Picture]
drawPowerUps h pups =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ Pictures
          [ color (makeColorI 255 255 255 40) $ rectangleSolid (cellSize*0.8) (cellSize*0.8)
          , color c $ circleSolid (cellSize / 4)
          ]
  | PowerUp (x,y) pType <- pups
  , let c = case pType of
              BombUp  -> yellow
              FlameUp -> orange
  ]

-- drawMonsters (Giữ nguyên)
drawMonsters :: Int -> [Monster] -> [Picture]
drawMonsters h ms =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ color (dark green) (rectangleSolid (cellSize*0.7) (cellSize*0.7))
  | Monster _ (x,y) <- ms
  ]

-- drawStatus (Giữ nguyên)
drawStatus :: GameStatus -> (Int, Int) -> Picture
drawStatus Playing _ = Blank
drawStatus Draw (w, h) = centerText w h "DRAW!"
drawStatus (GameOver 1) (w, h) = centerText w h "PLAYER 1 WINS!"
drawStatus (GameOver 2) (w, h) = centerText w h "PLAYER 2 WINS!"
drawStatus (GameOver pid) (w, h) = centerText w h ("PLAYER " ++ show pid ++ " WINS!")

-- centerText (Giữ nguyên)
centerText :: Int -> Int -> String -> Picture
centerText boardWidth boardHeight msg =
  let
    centerX = (fromIntegral boardWidth * cellSize) / 2
    centerY = (fromIntegral boardHeight * cellSize) / 2
    textWidth = fromIntegral (length msg) * 12
  in
    Translate (centerX - textWidth) (centerY - 10) $
    Scale 0.2 0.2 $
    Color white $
    Text msg


-- LỊCH SỬ CHAT: Tin mới ở TRÊN, chạy từ TRÊN XUỐNG
drawChatHistory :: [String] -> Picture
drawChatHistory msgs =
  let recentMsgs = take 6 msgs  -- KHÔNG REVERSE → tin mới ở CUỐI danh sách
      -- Vì server thêm tin vào ĐẦU: newMsg : oldMsgs
      -- → tin mới nhất = last (take 6 msgs)
  in Translate (-350) (-380) $  -- BẮT ĐẦU TỪ DƯỚI
     Scale 0.11 0.11 $
     Pictures
       [ Translate 0 (fromIntegral i * 140) $  -- VẼ TỪ DƯỚI LÊN
         Color (greyN 0.9) $
         Text (take 50 msg)
       | (i, msg) <- zip [0..] recentMsgs
       ]

-- INPUT: Dưới cùng, dưới dòng chat mới nhất
drawChatInput :: Bool -> String -> Picture
drawChatInput isTyping buffer =
  Translate (-350) (-400) $
  Scale 0.12 0.12 $
  Color white $
  if isTyping
  then Text ("> " ++ take 45 buffer ++ (if length buffer > 45 then "..." else "") ++ "_")
  else Text "[Enter để chat]"


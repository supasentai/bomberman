{-# LANGUAGE NamedFieldPuns #-}
module Render where

import Graphics.Gloss
import Types
import Debug.Trace
import Data.List (take)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Types (PlayerStatus(..))
cellSize :: Float
cellSize = 40

-- HÀM MỚI: Vẽ Menu (Sảnh chờ) - ĐÃ CẬP NHẬT TÊN
drawLobby :: Picture
drawLobby = Pictures
  [ Translate (-350) 50 $ Scale 0.3 0.3 $ Color white $ Text "BOMBERMAN"
  , Translate (-350) 0 $ Scale 0.15 0.15 $ Color yellow $ Text "CHON CHE DO:"
  , Translate (-350) (-50) $ Scale 0.1 0.1 $ Color white $ Text "1. Nguoi choi vs Nguoi (Co quai vat)"
  , Translate (-350) (-80) $ Scale 0.1 0.1 $ Color white $ Text "2. Nguoi choi vs May (Co quai vat)"
  ]

-- NÂNG CẤP: `drawGame` (Giờ kiểm tra `status` và gọi `drawStatus`)
drawGame :: GameState -> Map Int (Float, Float) -> Picture
drawGame gs visualPlayers =
  let
    gameWorld =
      case board gs of
        [] -> Blank -- Nếu board rỗng (như ở Lobby), không vẽ gì
        b@(firstRow:_) ->
          Scale 0.6 0.6 $
          Translate (-w'/2) (-h'/2) $
          Pictures $
            (concat
              [ [drawBoard b]
              , drawPlayers h (players gs) visualPlayers
              , drawBombs h (bombs gs)
              , drawFlames h (flames gs)
              , drawPowerUps h (powerups gs)
              , drawMonsters h (monsters gs)
              ])
            -- `drawStatus` sẽ xử lý Game Over HOẶC Lobby
            ++ [drawStatus (status gs) (w, h)]
          where
            w = length firstRow
            h = length b
            w' = fromIntegral w * cellSize
            h' = fromIntegral h * cellSize
    
    -- 2. MỚI: Vẽ trạng thái 2 players
    playerStatuses = 
      [ drawPlayerStatus w h p 
      | p <- take 2 (players gs)
      , let w = length (head (board gs))
            h = length (board gs)
      ]
    -- 3. Vẽ Timer
    timerUI = drawGamePhaseTimer (gamePhaseTimer gs)
    
    -- `drawStatus` sẽ tự quyết định vẽ Lobby hay Game Over
    statusUI = drawStatus (status gs) (0,0) 
  
  in
    -- Nếu là Lobby, chỉ vẽ statusUI (Menu). Nếu không, vẽ Game + Timer.
    if status gs == Lobby
    then statusUI
    else Pictures [gameWorld, Pictures playerStatuses, timerUI]

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
drawPlayers :: Int -> [Player] -> Map Int (Float, Float) -> [Picture]
drawPlayers h ps visualPlayers =
  [ 
    translate (visualX * cellSize) (fromIntegral (h - 1) * cellSize - visualY * cellSize)
      $ Pictures
          [ color (dark (greyN 0.1)) (translate 3 (-3) (circleSolid r))
          , if hasShield then color (makeColor 1 1 1 0.3) (circleSolid (r*1.5)) else Blank
          , color col (circleSolid r)
          ]
  | p@(Player pid _ True _ _ hasShield chaosT iframesT _) <- ps
  , let r = cellSize / 3
  , let 
        (visualX, visualY) = Map.findWithDefault (fromIntegral $ fst (pos p), fromIntegral $ snd (pos p)) pid visualPlayers
        isInvincible = chaosT > 0 || iframesT > 0
        blinkColor = if isInvincible && (round (iframesT * 10 + chaosT * 10) `mod` 2 == 0)
                     then yellow
                     else if pid == 1 then red else blue
        col = blinkColor
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
              Shield  -> white
              Chaos   -> magenta
  ]

-- drawMonsters (Giữ nguyên)
drawMonsters :: Int -> [Monster] -> [Picture]
drawMonsters h ms =
  [ translate (fromIntegral x * cellSize) 
              (fromIntegral (h - 1 - y) * cellSize)
      $ color col (rectangleSolid (cellSize*0.7) (cellSize*0.7))
  | Monster _ (x,y) mType <- ms
  , let col = case mType of
                Grunt -> dark green
                Ghost -> makeColor 200 100 255 255
  ]

-- NÂNG CẤP: `drawStatus` (Sửa logic `Lobby` và text thắng)
drawStatus :: GameStatus -> (Int, Int) -> Picture
drawStatus Playing _ = Blank
drawStatus Lobby _ = drawLobby -- SỬA LỖI: Vẽ Menu
drawStatus Draw (w, h) = centerText w h "DRAW!"
-- XÓA BỎ text thắng/thua quái vật (không còn dùng đến)
-- drawStatus (GameOver 100) (w, h) = centerText w h "PLAYERS WIN!"
-- drawStatus (GameOver 99) (w, h) = centerText w h "MONSTERS WIN!"

-- SỬA LẠI text cho chế độ 1v1
drawStatus (GameOver 1) (w, h) = centerText w h "PLAYER 1 WINS!"
drawStatus (GameOver 2) (w, h) = centerText w h "PLAYER 2 WINS!" -- (Áp dụng cho cả Người và Máy)
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

-- drawGamePhaseTimer (Giữ nguyên)
drawGamePhaseTimer :: Float -> Picture
drawGamePhaseTimer timer
  | timer <= 0 =
      Translate 250 280 $
      Scale 0.15 0.15 $
      Color red $
      Text "GHOSTS UNLEASHED!"
  | otherwise =
      Translate 300 280 $
      Scale 0.2 0.2 $
      Color white $
      Text (printf "%.1f" timer)

-- drawChatHistory (Giữ nguyên)
drawChatHistory :: [String] -> Picture
drawChatHistory msgs =
  let recentMsgs = take 6 msgs
  in Translate (-350) (-380) $
     Scale 0.11 0.11 $
     Pictures
       [ Translate 0 (fromIntegral i * 140) $
         Color (greyN 0.9) $
         Text (take 50 msg)
       | (i, msg) <- zip [0..] recentMsgs
       ]

-- drawChatInput (Giữ nguyên)
drawChatInput :: Bool -> String -> Picture
drawChatInput isTyping buffer =
  Translate (-350) (-400) $
  Scale 0.12 0.12 $
  Color white $
  if isTyping
  then Text ("> " ++ take 45 buffer ++ (if length buffer > 45 then "..." else "") ++ "_")
  else Text "[Enter để chat]"

getPlayerStatus :: Player -> PlayerStatus
getPlayerStatus p@Player{iframes, chaosTimer, hasShield, blastRadius} =
  if iframes > 0
    then PIframe iframes
    else if chaosTimer > 0
      then PChaos chaosTimer
      else if hasShield
        then PShield
        else PRadius blastRadius

-- Thêm hàm drawPlayerStatus
drawPlayerStatus :: Int -> Int -> Player -> Picture
drawPlayerStatus boardWidth boardHeight p =
  let
    status = getPlayerStatus p
    pidStr = "P" ++ show (playerId p)
    statusText = case status of
      PNormal      -> "NORMAL"
      PRadius r    -> printf "RADIUS: %d" r
      PShield      -> "SHIELD"
      PChaos t     -> printf "CHAOS: %.1f" t
      PIframe t    -> printf "INVULN: %.1f" t
    
    -- Kích thước FIXED - nhỏ gọn + cân đối
    boxWidth  = 190  -- ← FIXED
    boxHeight = 70   -- ← FIXED
    
    -- Vị trí CHÍNH GIỮA TRÊN CÙNG
    scaledWidth  = fromIntegral boardWidth  * cellSize * 0.6
    scaledHeight = fromIntegral boardHeight * cellSize * 0.6
    centerX      = scaledWidth / 2  -180        -- = 180px
    topY         = scaledHeight / 2 + 35    -- Trên board 35px
    
    -- P1 trái (-95px), P2 phải (+95px) → TỔNG 190px (cân đối)
    offsetX = if playerId p == 1 then -95 else 95
    finalX  = centerX + offsetX
    finalY  = topY
    
    -- Background + viền
    bg      = color (makeColor 0 0 0 0.9) $ rectangleSolid boxWidth boxHeight
    border  = color white $ rectangleWire (boxWidth+2) (boxHeight+2)
    
    playerColor = if playerId p == 1 then red else blue
    
    -- ICON ● CĂN GIỮA HOÀN HẢO (x = -55)
    icon     = translate (-55) 2  $ color playerColor $ circleSolid 18
    
    -- TEXT CĂN CHUẨN (icon ● + text sát nhau)
    pidPic    = translate (-28) 10  $ scale 0.22 0.22 $ color white $ Text pidStr
    statusPic = translate (-28) (-20) $ scale 0.16 0.16 $ color yellow $ Text statusText
    
  in
    Translate finalX finalY $ Pictures [bg, border, icon, pidPic, statusPic]
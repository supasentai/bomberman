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

spriteSize :: Float
spriteSize = 40.0

-- Tỉ lệ co giãn 
scaleFactor :: Float
scaleFactor = 0.8

-- drawLobby
drawLobby :: Picture
drawLobby = Pictures
  [ Translate (-350) 50 $ Scale 0.3 0.3 $ Color white $ Text "BOMBERMAN"
  , Translate (-350) 0 $ Scale 0.15 0.15 $ Color yellow $ Text "CHON CHE DO:"
  , Translate (-350) (-50) $ Scale 0.1 0.1 $ Color white $ Text "1. Nguoi choi vs Nguoi"
  , Translate (-350) (-80) $ Scale 0.1 0.1 $ Color white $ Text "2. Nguoi choi vs May"
  ]

-- drawGame 
drawGame :: GameAssets -> GameState -> Map Int (Float, Float) -> Picture
drawGame assets gs visualPlayers =
  let
    gameWorld =
      case board gs of
        [] -> Blank
        b@(firstRow:_) ->
          Scale scaleFactor scaleFactor $
          Translate (-w'/2) (-h'/2) $ 
          Pictures $
            (concat
              [ [drawBoard assets b]
              , drawPlayers assets h (players gs) visualPlayers
              , drawBombs assets h (bombs gs)
              , drawFlames assets h (flames gs)
              , drawPowerUps assets h (powerups gs)
              , drawMonsters assets h (monsters gs)
              ] ++ [drawStatus (status gs) (w, h)])
            where
              w = length firstRow
              h = length b
              -- Tính toán kích thước thật của board (dựa trên kích thước sprite)
              w' = fromIntegral w * spriteSize
              h' = fromIntegral h * spriteSize
    
    -- UI: Bảng trạng thái Player
    playerStatuses = 
      [ drawPlayerStatus scaleFactor w h p 
      | p <- take 2 (players gs)
      , not (null (board gs)) -- Chỉ vẽ khi có board
      , let w = length (head (board gs))
            h = length (board gs)
      ]
    
    -- UI: Đồng hồ đếm ngược
    timerUI = drawGamePhaseTimer (gamePhaseTimer gs)
  
  in
    if status gs == Lobby
    then drawStatus (status gs) (0,0)
    else
      -- Vẽ gameWorld (đã tự căn giữa) và các UI xung quanh
      Pictures [gameWorld, Pictures playerStatuses, timerUI]

-- drawBoard 
drawBoard :: GameAssets -> Board -> Picture
drawBoard assets b =
  Pictures
    [ translate (fromIntegral x * spriteSize) (fromIntegral y * spriteSize)
        (drawCell assets c)
    | (y, row) <- zip [0..] (reverse b)
    , (x, c) <- zip [0..] row
    ]

-- drawCell
drawCell :: GameAssets -> Cell -> Picture
drawCell assets Wall  = pWall assets
drawCell assets Box   = pBox assets
drawCell assets Empty = pEmpty assets


-- drawPlayers
drawPlayers :: GameAssets -> Int -> [Player] -> Map Int (Float, Float) -> [Picture]
drawPlayers assets h ps visualPlayers =
  [ 
    translate (visualX * spriteSize) (fromIntegral (h - 1) * spriteSize - visualY * spriteSize)
      $ Pictures
          [ -- 1. Sprite người chơi
            if playerId p == 1 then pPlayer1 assets else pPlayer2 assets
            
            -- 2. Hiệu ứng Shield (bong bóng xanh)
          , if hasShield then color (makeColor 1 1 1 0.3) (circleSolid (spriteSize*0.6)) else Blank
            
            -- 3. Hiệu ứng Chaos (vòng tròn vàng nhấp nháy)
          , if isChaos && isFlashingChaos
            then color (makeColor 1 1 0 0.5) (circleSolid (spriteSize * 0.6)) 
            else Blank
            
            -- 4. Hiệu ứng I-Frame (sprite khiên vỡ nhấp nháy)
          , if isIFrame && isFlashingIFrame
            then pBrokenShield assets 
            else Blank
          ]
  | p@(Player pid _ True _ _ hasShield chaosT iframesT _) <- ps
  , let 
        (visualX, visualY) = Map.findWithDefault (fromIntegral $ fst (pos p), fromIntegral $ snd (pos p)) pid visualPlayers
        
        isChaos = chaosT > 0
        isIFrame = iframesT > 0
        
        isFlashingChaos = (round (chaosT * 10) `mod` 2 == 0)
        isFlashingIFrame = (round (iframesT * 10) `mod` 2 == 0)
  ]

-- drawBombs
drawBombs :: GameAssets -> Int -> [Bomb] -> [Picture]
drawBombs assets h bs =
  [ translate (fromIntegral x * spriteSize) 
              (fromIntegral (h - 1 - y) * spriteSize)
      $ Pictures
          [ 
            Scale s s $ pBomb assets
            
          , if isAboutToExplode
            then color (makeColor 1 1 1 0.7) (circleSolid (spriteSize * 0.5))
            else Blank
          ]
  | Bomb (x,y) t _ _ <- bs
  , let s = 0.8 + 0.4 * (t / 2.0) 
  , let isAboutToExplode = t < 0.5 && (round (t * 20) `mod` 2 == 0)
  ]

-- drawFlames
drawFlames :: GameAssets -> Int -> [Flame] -> [Picture]
drawFlames assets h fs =
  [ translate (fromIntegral x * spriteSize) 
              (fromIntegral (h - 1 - y) * spriteSize)
      $ Pictures
          [ 
            Scale scaleFactor scaleFactor $
            color (makeColor 1 1 1 alpha) $
            pFlame assets
          ]
  | Flame (x,y) r <- fs
  , let flameDuration = 0.5 
  , let scaleFactor = max 0 (r / flameDuration)
  , let alpha = max 0 (r / flameDuration)
  ]

-- drawPowerUps
drawPowerUps :: GameAssets -> Int -> [PowerUp] -> [Picture]
drawPowerUps assets h pups =
  [ translate (fromIntegral x * spriteSize) 
              (fromIntegral (h - 1 - y) * spriteSize)
      $ spriteFor pType
  | PowerUp (x,y) pType <- pups
  , let spriteFor BombUp  = pBombUp assets
        spriteFor FlameUp = pFlameUp assets
        spriteFor Shield  = pShield assets
        spriteFor Chaos   = pChaos assets
  ]

-- drawMonsters
drawMonsters :: GameAssets -> Int -> [Monster] -> [Picture]
drawMonsters assets h ms =
  [ translate (fromIntegral x * spriteSize) 
              (fromIntegral (h - 1 - y) * spriteSize)
      $ spriteFor mType
  | Monster _ (x,y) mType <- ms
  , let spriteFor Grunt = pMonster assets
        spriteFor Ghost = color (makeColor 1 1 1 0.7) (pGhost assets)
  ]

-- drawStatus
drawStatus :: GameStatus -> (Int, Int) -> Picture
drawStatus Playing _ = Blank
drawStatus Lobby _ = drawLobby
drawStatus Draw (w, h) = centerText w h "DRAW!"
drawStatus (GameOver 1) (w, h) = centerText w h "PLAYER 1 WINS!"
drawStatus (GameOver 2) (w, h) = centerText w h "PLAYER 2 WINS!"
drawStatus (GameOver pid) (w, h) = centerText w h ("PLAYER " ++ show pid ++ " WINS!")


-- centerText
centerText :: Int -> Int -> String -> Picture
centerText boardWidth boardHeight msg =
  let
    centerX = (fromIntegral boardWidth * spriteSize) / 2
    centerY = (fromIntegral boardHeight * spriteSize) / 2
    textWidth = fromIntegral (length msg) * 12
  in
    Translate (centerX - textWidth) (centerY - 10) $
    Scale 0.2 0.2 $
    Color white $
    Text msg

-- drawGamePhaseTimer
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

-- drawChatHistory
drawChatHistory :: [String] -> Picture
drawChatHistory msgs =
  let recentMsgs = take 6 msgs
  in Translate (-260) (-380) $ 
     Scale 0.11 0.11 $
     Pictures
       [ Translate 0 (fromIntegral i * 140) $
         Color (greyN 0.9) $
         Text (take 50 msg)
       | (i, msg) <- zip [0..] recentMsgs
       ]

-- drawChatInput
drawChatInput :: Bool -> String -> Picture
drawChatInput isTyping buffer =
  Translate (-260) (-400) $ 
  Scale 0.12 0.12 $
  Color white $
  if isTyping
  then Text ("> " ++ take 45 buffer ++ (if length buffer > 45 then "..." else "") ++ "_")
  else Text "[Enter để chat]"

-- getPlayerStatus
getPlayerStatus :: Player -> PlayerStatus
getPlayerStatus p@Player{iframes, chaosTimer, hasShield, blastRadius} =
  if iframes > 0
    then PIframe iframes
    else if chaosTimer > 0
      then PChaos chaosTimer
      else if hasShield
        then PShield
        else PRadius blastRadius

-- drawPlayerStatus
drawPlayerStatus :: Float -> Int -> Int -> Player -> Picture
drawPlayerStatus scaleFactor boardWidth boardHeight p =
  let
    pidStr = "P" ++ show (playerId p)
    radiusText = printf "RADIUS: %d" (blastRadius p)
    bombsText  = printf "BOMBS: %d/%d" (maxBombs p) (maxBombs p)
    shieldText = if hasShield p then "SHIELD: ON" else "SHIELD: OFF"
    chaosText  = printf "CHAOS: %.1fs" (chaosTimer p)
    invulnText = printf "INVULN: %.1fs" (iframes p)
    
    statusLines = [ radiusText , bombsText , shieldText , chaosText , invulnText ]
    
    boxWidth  = 180
    boxHeight = 140
    
    -- ĐÃ KHÔI PHỤC LOGIC GỐC
    -- Tính toán vị trí dựa trên kích thước đã scale
    scaledWidth  = fromIntegral boardWidth  * spriteSize * scaleFactor
    scaledHeight = fromIntegral boardHeight * spriteSize * scaleFactor
    centerX      = scaledWidth / 2  - 200 -- Lệch 100
    topY         = scaledHeight / 2 - 90  -- Lệch 90
    
    -- Đặt P1 bên trái, P2 bên phải
    offsetX = if playerId p == 1 then -400 else 290  
    finalX  = centerX + offsetX
    finalY  = topY
    -- KẾT THÚC LOGIC GỐC
    
    bg      = color (makeColor 0 0 0 0.95) $ rectangleSolid boxWidth boxHeight
    border  = color white $ rectangleWire (boxWidth + 4) (boxHeight + 4)
    
    playerColor = if playerId p == 1 then red else blue
    icon     = translate (-68) 54   $ color playerColor $ circleSolid 16
    pidPic   = translate (-50) 46   $ scale 0.18 0.18 $ color white $ Text pidStr
    
    statusPics = 
      [ translate (-72) (12 - fromIntegral i * 18) $
        scale 0.12 0.12 $                           
        color (if i < 2 then yellow else white) $   
        Text line
      | (i, line) <- zip [0..4] statusLines
      ]
    
  in
    Translate finalX finalY $ Pictures [bg, border, icon, pidPic] <> Pictures statusPics
module Render (renderGame) where

import Types
import System.Console.ANSI
import Control.Monad (forM_)

renderGame :: GameState -> IO ()
renderGame gs = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "==== 🧨 BOMBERMAN HASKELL 🧨 ====\n"
  renderBoard (board gs) (players gs) (bombs gs)
  putStrLn $ "\nTick: " ++ show (tick gs)

------------------------------------------------------------
-- Vẽ bản đồ
------------------------------------------------------------
renderBoard :: Board -> [Player] -> [Bomb] -> IO ()
renderBoard b ps bs = do
  forM_ (zip [0..] b) $ \(y, row) -> do
    forM_ (zip [0..] row) $ \(x, cell) -> do
      let pHere = findPlayer (x, y) ps
          bHere = findBomb (x, y) bs
      case (pHere, bHere, cell) of
        (Just p, _, _) -> renderPlayer p
        (_, Just _, _) -> renderBomb
        _              -> renderCell cell
    putStrLn ""

------------------------------------------------------------
-- Các hàm phụ
------------------------------------------------------------
findPlayer (x, y) = foldr (\p acc -> if pos p == (x, y) then Just p else acc) Nothing
findBomb (x, y) = foldr (\b acc -> if bPos b == (x, y) then Just b else acc) Nothing

renderCell Wall = setSGR [SetColor Foreground Vivid Blue] >> putStr "██"
renderCell SoftBlock = setSGR [SetColor Foreground Dull Yellow] >> putStr "▒▒"
renderCell Empty = setSGR [Reset] >> putStr "  "
renderCell Flame = setSGR [SetColor Foreground Vivid Red] >> putStr "🔥"

renderPlayer p =
  case pType p of
    Human -> setSGR [SetColor Foreground Vivid Green] >> putStr "😀"
    AI    -> setSGR [SetColor Foreground Vivid Magenta] >> putStr "🤖"

renderBomb = setSGR [SetColor Foreground Vivid Red] >> putStr "💣"

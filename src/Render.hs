{-# LANGUAGE RecordWildCards #-}

module Render where

import Graphics.Gloss
import Types

cellSize :: Float
cellSize = 50

drawGame :: GameState -> IO Picture
drawGame GameState{..} = do
  let pPics = [ translate (fromIntegral x * cellSize)
                        (fromIntegral y * cellSize)
                        (color (if pid == 1 then blue else red)
                          (circleSolid (cellSize/4)))
              | Player{..} <- players
              , let (x,y) = pos ]
      bPics = [ translate (fromIntegral x * cellSize)
                        (fromIntegral y * cellSize)
                        (color orange (circleSolid (cellSize/6)))
              | (x,y) <- bombs ]
  return $ pictures (pPics ++ bPics)

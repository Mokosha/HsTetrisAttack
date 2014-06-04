module TetrisAttack.Constants (
  screenSizeX, screenSizeY, screenSizeXf, screenSizeYf, halfScreenSizeXf, halfScreenSizeYf,
  blocksPerRow, rowsPerBoard,
  blockSize, boardSizeX, boardSizeY, boardSizeXf, boardSizeYf, halfBoardSizeXf, halfBoardSizeYf,
  boardOrigin, boardCenter, blockOrigin, blockCenter,
  RenderLayer(..), renderDepth,
  gSwapSpeed
) where

--------------------------------------------------------------------------------

import Linear.Vector
import Linear.V2

--------------------------------------------------------------------------------

screenSizeX :: Int
screenSizeX = 1024

screenSizeY :: Int
screenSizeY = 768

screenSizeXf :: Float
screenSizeXf = fromIntegral screenSizeX

screenSizeYf :: Float
screenSizeYf = fromIntegral screenSizeY

halfScreenSizeXf :: Float
halfScreenSizeXf = 0.5 * screenSizeXf

halfScreenSizeYf :: Float
halfScreenSizeYf = 0.5 * screenSizeYf

blockSize :: Int
blockSize = 35

blocksPerRow :: Int
blocksPerRow = 6

rowsPerBoard :: Int
rowsPerBoard = 20

boardSizeX :: Int
boardSizeX = blocksPerRow * blockSize

boardSizeY :: Int
boardSizeY = rowsPerBoard * blockSize

boardSizeXf :: Float
boardSizeXf = fromIntegral boardSizeX

boardSizeYf :: Float
boardSizeYf = fromIntegral boardSizeY

halfBoardSizeXf :: Float
halfBoardSizeXf = 0.5 * boardSizeXf

halfBoardSizeYf :: Float
halfBoardSizeYf = 0.5 * boardSizeYf

boardOrigin :: V2 Int
boardOrigin = let
  halfScreenX = screenSizeX `div` 2
  halfScreenY = screenSizeY `div` 2
  halfBoardX = boardSizeX `div` 2
  halfBoardY = boardSizeY `div` 2
 in
   V2 (halfScreenX - halfBoardX) (halfScreenY - halfBoardY)

blockOrigin :: (Int, Int) -> V2 Int
blockOrigin (x, y) = blockSize *^ (V2 (x - 1) (y - 1)) ^+^ boardOrigin

blockCenter :: (Int, Int) -> V2 Float
blockCenter loc = let
  (V2 x y) = fmap fromIntegral (blockOrigin loc)
  in
   V2 (0.5 * (fromIntegral blockSize) + x) (0.5 * (fromIntegral blockSize) + y)

boardCenter :: (Int, Int)
boardCenter = ((blocksPerRow - 1) `div` 2, (rowsPerBoard - 1) `div` 2)

data RenderLayer = RenderLayer'Board
                 | RenderLayer'Tiles
                 | RenderLayer'Cursor

renderDepth :: RenderLayer -> Float
renderDepth RenderLayer'Board = (-10)
renderDepth RenderLayer'Tiles = (-9)
renderDepth RenderLayer'Cursor = (-8)

--------------------------------------------------------------------------------

-- Gameplay constants

gSwapSpeed :: Float
gSwapSpeed = 0.2

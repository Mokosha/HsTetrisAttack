module TetrisAttack.AI (
  aiCommands
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))

import Data.Foldable (maximumBy)
import Data.Function (on)

import qualified Lambency as L

import System.Random

import TetrisAttack.Board.Types
import TetrisAttack.Board.Combos
import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

canSwap :: Tile -> Bool
canSwap Blank = True
canSwap Vanished = True
canSwap (Stationary _) = True
canSwap (Falling True _ _) = True
canSwap _ = False

swapTiles :: Cursor -> BoardState -> BoardState
swapTiles (_, False) bs = bs
swapTiles ((x, y), True) bs =
  let leftPos = (x, y)
      rightPos = (x + 1, y)

      leftTile = get2D leftPos bs
      rightTile = get2D rightPos bs

      (newLeft, newRight) =
        case (canSwap leftTile, canSwap rightTile) of
          (True, True) -> (rightTile, leftTile)
          _ -> (leftTile, rightTile)
  in
   update2D newLeft leftPos $
   update2D newRight rightPos $
   bs

heightWalker :: GridWalker Tile Int
heightWalker = heightFrom (0, 1)
  where
    walkFn :: (Int, Int) -> Maybe Tile -> GridWalker Tile Int
    walkFn (h, _) Nothing = Result h
    walkFn (h, r) (Just Vanished) = heightFrom (h, r + 1)
    walkFn (h, r) (Just Blank) = heightFrom (h, r + 1)
    walkFn (h, r) (Just FallingOut) = heightFrom (h, r + 1)
    walkFn (h, r) (Just SwappedOut) = heightFrom (h, r + 1)
    walkFn (_, r) _ = heightFrom (r, r + 1)

    heightFrom :: (Int, Int) -> GridWalker Tile Int
    heightFrom = Walker . walkFn

nextCommand :: CursorCommand -> Cursor -> BoardState -> CursorCommand
nextCommand fallback cur@((x, y), _) bs =
  let allPositions :: [Cursor]
      allPositions = [((x', y'), True) | x' <- [1..(blocksPerRow - 1)], y' <- [1..rowsPerBoard]]

      potentialBoards = map (flip swapTiles bs) allPositions

      potentialCombos = zip allPositions $ map (length . gatherCombos) potentialBoards

      (((tx, ty), _), numCombos) = maximumBy (compare `on` snd) potentialCombos

      boardHeight = maximum $ walkColumns bs heightWalker

      ((_, fby), _) = commandToCursor cur fallback
  in
   if numCombos == 0 then
     if fby > boardHeight then
       CursorCommand'MoveDown
     else
       fallback
   else if (tx, ty) == (x, y) then
          CursorCommand'Swap
        else if (abs (tx - x) > abs (ty - y)) then
               if tx > x then
                 CursorCommand'MoveRight
               else
                 CursorCommand'MoveLeft
             else
               if ty > y then
                 CursorCommand'MoveUp
               else
                 CursorCommand'MoveDown

randomFeedback :: RandomGen g => L.GameWire ((Cursor, BoardState), g) (CursorCommand, g)
randomFeedback = second (arr random) >>>
                 (arr $ \((c, bs), (fb, gen)) -> (nextCommand fb c bs, gen))

aiCommands :: RandomGen g => g -> L.GameWire (Cursor, BoardState) [CursorCommand]
aiCommands gen = (loop $ second (delay gen) >>> randomFeedback) >>> arr pure

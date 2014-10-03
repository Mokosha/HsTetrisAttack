module TetrisAttack.AI (
  aiCommands
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))

import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)

import qualified Lambency as L

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

collapseWalker :: GridWalker Tile [Tile]
collapseWalker = Walker $ collapseWalker' []
  where
    collapseWalker' accum Nothing = Result accum
    collapseWalker' accum (Just (Stationary c)) = Walker $ collapseWalker' $ (Stationary c) : accum
    collapseWalker' accum (Just (Falling _ _ c)) = Walker $ collapseWalker' $ (Stationary c) : accum
    collapseWalker' accum (Just (Moving c)) = Walker $ collapseWalker' $ (Stationary c) : accum
    collapseWalker' accum (Just _) = Walker $ collapseWalker' accum

collapse :: BoardState -> BoardState
collapse bs = fromLists $ map (take rowsPerBoard . (++ (repeat Blank))) $ walkColumnsRev bs collapseWalker

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

posDist :: GridLocation2D -> GridLocation2D -> Int
posDist (x, y) (x', y') =
  let dx = x - x'
      dy = y - y'
  in dx * dx + dy * dy

allPositions :: [Cursor]
allPositions = [((x', y'), True) | x' <- [1..(blocksPerRow - 1)], y' <- [1..rowsPerBoard]]

potentialBoards :: [Cursor] -> BoardState -> [BoardState]
potentialBoards curs bs = map (collapse . flip swapTiles bs) curs

boardHeights :: BoardState -> [Int]
boardHeights = flip walkColumns heightWalker

moveCursorToTarget :: Cursor -> GridLocation2D -> CursorCommand
moveCursorToTarget ((x, y), _) (tx, ty) =
  if (tx, ty) == (x, y) then
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

avg :: [Int] -> Int
avg l = (foldl1 (+) l) `div` (length l)

{--
triageStacks :: Cursor -> BoardState -> CursorCommand
triageStacks cur bs =
--}

selectBestCombo :: Cursor -> BoardState -> CursorCommand
selectBestCombo cur@((x, y), _) bs =
  let boards = potentialBoards (((x, y), False) : allPositions) bs

      countCombos :: Int -> [(Cursor, Int)]
      countCombos num = zip (((x, y), False) : allPositions) $
                        map (length . gatherCombos num) boards

      potentialCombos = countCombos 3
      potentialPairs = countCombos 2

      selectClosest :: Int -> [(Cursor, Int)] -> GridLocation2D
      selectClosest num combos = minimumBy (compare `on` (posDist (x, y))) $ -- Select the closest
                                 map (fst . fst) . filter ((== num) . snd) $ -- that has num combos
                                 combos

      (_, maxCombos) = maximumBy (compare `on` snd) potentialCombos

      target =
        if maxCombos == 0 then
          let (_, maxPairs) = maximumBy (compare `on` snd) potentialPairs
          in selectClosest maxPairs potentialPairs
        else
          selectClosest maxCombos potentialCombos
  in
   moveCursorToTarget cur target

nextCommand :: Cursor -> BoardState -> CursorCommand
nextCommand cur bs =
  let heights = boardHeights bs
      height = maximum heights
  in
   if height > avg heights + 5
   then selectBestCombo cur bs -- triageStacks cur bs
   else selectBestCombo cur bs

aiCommands :: L.GameWire (Cursor, BoardState) [CursorCommand]
aiCommands = arr $ pure . uncurry nextCommand

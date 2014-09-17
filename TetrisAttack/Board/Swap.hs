module TetrisAttack.Board.Swap (
  swapTiles
) where

--------------------------------------------------------------------------------
import Control.Wire

import TetrisAttack.Board.Types
import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

swappingWire :: TileLogic a
swappingWire = (pure (\_ -> return SwappedOut) >>> (for gSwapDelay)) --> blank

swapTiles :: TileMap -> Cursor -> (BoardState, Board a) -> Board a
swapTiles _ (_, False) (_, b) = b
swapTiles m ((x, y), True) (st, board) = let

  leftPos = (x, y)
  rightPos = (x + 1, y)

  leftTile = get2D leftPos st
  rightTile = get2D rightPos st

  (leftLogic, rightLogic) = case (rightTile, leftTile) of
    (Stationary rcolor, Stationary lcolor) -> (swapping m rcolor False, swapping m lcolor True)
    (Blank, Stationary lcolor) -> (swappingWire, swapping m lcolor True)
    (Stationary rcolor, Blank) -> (swapping m rcolor False, swappingWire)
    (Falling True _ rcolor, Falling True _ lcolor) ->
      (swapping m rcolor False, swapping m lcolor True)
    (Blank, Falling True _ lcolor) -> (swappingWire, swapping m lcolor True)
    (Falling True _ rcolor, Blank) -> (swapping m rcolor False, swappingWire)
    _ -> (get2D leftPos board, get2D rightPos board)
  in
   update2D leftLogic leftPos $ update2D rightLogic rightPos board


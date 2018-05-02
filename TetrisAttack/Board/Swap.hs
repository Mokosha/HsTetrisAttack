module TetrisAttack.Board.Swap (
  swapTiles
) where

--------------------------------------------------------------------------------
import Control.Wire

import Prelude hiding (id, (.))

import TetrisAttack.Board.Types
import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

swappingWire :: TileLogic a
swappingWire = (for gSwapDelay . pure SwappedOut) --> blank

-- If the tile is being switched, what is the wire that it will turn into...
wireMap :: TileMap -> Tile -> Bool -> Maybe (TileLogic a)
wireMap m (Stationary rcolor) False = Just $ swapping m rcolor False
wireMap m (Stationary lcolor) True = Just $ swapping m lcolor True
wireMap _ Blank _ = Just swappingWire
wireMap _ Vanished _ = Just swappingWire
wireMap _ _ _ = Nothing

isStationary :: Tile -> Bool
isStationary (Stationary _) = True
isStationary _ = False

swapTiles :: TileMap -> Cursor -> (BoardState, Board a) -> Board a
swapTiles _ (_, False) (_, b) = b
swapTiles m ((x, y), True) (st, board) = let

  leftPos = (x, y)
  rightPos = (x + 1, y)

  leftTile = get2D leftPos st
  rightTile = get2D rightPos st

  originalLogic = (get2D leftPos board, get2D rightPos board)

  (leftLogic, rightLogic) =
    case (wireMap m rightTile False, wireMap m leftTile True) of
      (_, Nothing) -> originalLogic
      (Nothing, _) -> originalLogic
      (Just r, Just l) ->
        if (isStationary leftTile) || (isStationary rightTile)
        then (r, l)
        else originalLogic
  in
   update2D leftLogic leftPos $ update2D rightLogic rightPos board


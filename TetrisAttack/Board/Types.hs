module TetrisAttack.Board.Types where

--------------------------------------------------------------------------------
import TetrisAttack.Tile
import TetrisAttack.Grid
--------------------------------------------------------------------------------

type Board a = Grid2D (TileLogic a)
type BoardState = Grid2D Tile


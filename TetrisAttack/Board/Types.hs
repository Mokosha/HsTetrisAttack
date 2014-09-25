module TetrisAttack.Board.Types where

--------------------------------------------------------------------------------
import TetrisAttack.Tile
import TetrisAttack.Grid
--------------------------------------------------------------------------------

type Board a = Grid2D (TileLogic a)
type BoardState = Grid2D Tile

boardState2Board :: TileMap -> BoardState -> Board a
boardState2Board m = mapGrid convertTile
  where
    convertTile :: Tile -> TileLogic a
    convertTile (Stationary c) = stationary m c
    convertTile _ = blank

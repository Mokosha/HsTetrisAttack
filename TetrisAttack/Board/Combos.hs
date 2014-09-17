module TetrisAttack.Board.Combos (
  handleCombos
) where

--------------------------------------------------------------------------------
import Data.List (nub)

import TetrisAttack.Board.Types
import TetrisAttack.Constants
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

type Combo = (GridLocation2D, TileColor)
handleCombos :: TileMap -> (BoardState, Board a) -> (BoardState, Board a)
handleCombos m (st, b) = (bulkUpdate2D Vanishing (map fst gatheredTiles) st,
                          foldr updateLogic b gatheredTiles)
  where
    updateLogic :: Combo -> Board a -> Board a
    updateLogic (loc, color) = update2D (vanishing m color) loc

    countWalker :: GridWalker Tile [(Int, TileColor, Int)]
    countWalker = countHelper 0 1 Blank []
      where 
        countHelper :: Int -> Int -> Tile -> [(Int, TileColor, Int)] ->
                       GridWalker Tile [(Int, TileColor, Int)]
        countHelper pos cnt tile combos = Walker walkerFn
          where
            dump :: Tile -> TileColor -> GridWalker Tile [(Int, TileColor, Int)]
            dump t c = countHelper (pos + 1) 1 t ((pos, c, cnt) : combos)

            reset :: Tile -> GridWalker Tile [(Int, TileColor, Int)]
            reset t = countHelper (pos + 1) 1 t combos

            walkerFn :: Maybe Tile -> GridWalker Tile [(Int, TileColor, Int)]
            walkerFn Nothing
              | cnt >= 3 = case tile of
                (Stationary old) -> Result $ (pos, old, cnt) : combos
                _ -> error "handleCombos -- The impossible happened"
              | otherwise = Result $ combos

            walkerFn (Just (Stationary new)) =
              case tile of
                (Stationary old)
                  | old == new -> countHelper (pos + 1) (cnt + 1) (Stationary new) combos
                  | cnt >= 3 -> dump (Stationary new) old
                  | otherwise -> reset (Stationary new)
                _ -> reset (Stationary new)

            walkerFn (Just t)
              | cnt >= 3 =
                case tile of
                  (Stationary old) -> dump t old
                  _ -> reset t
              | otherwise = reset t

    mkRowCombo :: [(Int, TileColor, Int)] -> Int -> [(Combo, Int)]
    mkRowCombo l col = map (\(row, t, n) -> (((row, col), t), n)) l

    mkColCombo :: [(Int, TileColor, Int)] -> Int -> [(Combo, Int)]
    mkColCombo l row = map (\(col, t, n) -> (((row, col), t), n)) l

    expandRows :: (Combo, Int) -> [Combo]
    expandRows (((x, y), color), num) = [((col, y), color) | col <- [(x-num+1)..x]]

    gatheredRows :: [Combo]
    gatheredRows =
      (concat $ zipWith mkRowCombo (walkRows st countWalker) [1..rowsPerBoard]) >>= expandRows

    expandCols :: (Combo, Int) -> [Combo]
    expandCols (((x, y), c), num) = [((x, row), c) | row <- [(y-num+1)..y]]

    gatheredCols :: [Combo]
    gatheredCols =
      (concat $ zipWith mkColCombo (walkColumns st countWalker) [1..blocksPerRow]) >>= expandCols

    gatheredTiles :: [Combo]
    gatheredTiles = nub $ gatheredCols ++ gatheredRows

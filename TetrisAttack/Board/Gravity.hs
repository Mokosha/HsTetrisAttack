module TetrisAttack.Board.Gravity (
  handleGravity
) where

--------------------------------------------------------------------------------
import Control.Monad.State
import Control.Wire

import qualified Data.Vector as V

import TetrisAttack.Board.Types
import TetrisAttack.Constants
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

newlyFalling :: TileLogic a
newlyFalling = (pure (\_ -> return FallingOut) >>> (for gTileFallTime)) --> blank

handleGravity :: TileMap -> (BoardState, Board a) -> (BoardState, Board a)
handleGravity m (bs, b) = unzipGrid (V.map handleCol $ zipGrid bs b)
  where
    handleCol :: V.Vector (Tile, TileLogic a) -> V.Vector (Tile, TileLogic a)
    handleCol vec = evalState (V.generateM rowsPerBoard genCol) False
      where
        -- genCol :: Int -> State Bool (Tile, TileLogic a)
        genCol row
          -- Last row, just check if falling
          | row == (rowsPerBoard - 1) = do
            isFalling <- get
            if isFalling
              then return (FallingOut, newlyFalling)
              else return (vec V.! row)

          -- If it's not the last row, then first see what the top tile *would* be
          -- and then if we're going to fall, replace the current tile with the tile
          -- that the top tile turns into...
          | otherwise = let

            tileOnTop = case vec V.! (row + 1) of
              -- If the tile on top is stationary, then it will begin falling
              (Stationary c, _) ->
                Just (Falling False (fromIntegral blockSize) c, falling m c)

              -- If the tile on top is falling and about to become stationary,
              -- then it will keep falling
              (Falling True x c, _) ->
                Just (Falling False (fromIntegral blockSize + x) c, stillFalling m c x)

              -- If the tile does not meet any of the above criteria, it shouldn't
              -- fall down to the next level
              _ -> Nothing

            -- We can fall into this tile, so check if anything falls into it,
            -- and if not, then become the otherwiseTile
            checkTop :: (Tile, TileLogic a) -> State Bool (Tile, TileLogic a)
            checkTop otherwiseTile =
              case tileOnTop of
                Nothing -> do { put False; return otherwiseTile }
                Just tile -> do { put True; return tile }

           in do
              isFalling <- get
              case vec V.! row of
                -- If this tile is blank, then the tile above can fall into it.
                (Blank, _) -> checkTop (Blank, blank)

                -- If this tile is not blank, but is falling itself, then the tile
                -- above can fall into it.
                x | isFalling -> checkTop (FallingOut, newlyFalling)

                -- If this tile is not blank and is not falling, then the tile above
                -- cannot fall into it, and the tile remains stationary
                  | otherwise -> return x

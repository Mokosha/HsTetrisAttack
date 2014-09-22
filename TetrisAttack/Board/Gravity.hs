module TetrisAttack.Board.Gravity (
  handleGravity
) where

--------------------------------------------------------------------------------
import Control.Wire

import TetrisAttack.Board.Types
import TetrisAttack.Constants
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

type CombinedTile a = (Tile, TileLogic a)
type GravityUpdater a = GridUpdater (CombinedTile a) (CombinedTile a, Maybe (CombinedTile a))

genFallingTile :: TileMap -> CombinedTile a -> CombinedTile a ->
                  ((CombinedTile a, Maybe (CombinedTile a)), GravityUpdater a)
genFallingTile m f t = ((t, Just f), gravity m t)

gravity :: TileMap -> CombinedTile a -> GravityUpdater a
gravity m (Stationary c, _) =
  GridUpdater $ genFallingTile m (Falling False blockSizeN c, falling m c)
gravity m (Falling True x c, _) =
  GridUpdater $ genFallingTile m (Falling False (blockSizeN + x) c, stillFalling m c x)
gravity m _ = GridUpdater $ \tile -> ((tile, Nothing), gravity m tile)

gravityUpdater :: TileMap -> GravityUpdater a
gravityUpdater m = gravity m (Blank, blank)

type FallingUpdater a = GridUpdater (CombinedTile a, Maybe (CombinedTile a)) (CombinedTile a)

newlyFalling :: TileLogic a
newlyFalling = (pure (\_ -> return FallingOut) >>> (for gTileFallTime)) --> blank

updateFalling :: (CombinedTile a, Maybe (CombinedTile a)) -> (CombinedTile a, FallingUpdater a)
updateFalling ((Blank, _), Nothing) = ((Blank, blank), resolveFalling True)
updateFalling (_, Nothing) = ((FallingOut, newlyFalling), resolveFalling False)
updateFalling (_, Just ft) = (ft, resolveFalling True)

updateNotFalling :: (CombinedTile a, Maybe (CombinedTile a)) ->
                    (CombinedTile a, FallingUpdater a)
updateNotFalling ((Blank, _), Nothing) = ((Blank, blank), resolveFalling False)
updateNotFalling ((Blank, _), (Just ft)) = (ft, resolveFalling True)
updateNotFalling (t, _) = (t, resolveFalling False)

resolveFalling :: Bool -> FallingUpdater a
resolveFalling True = GridUpdater updateFalling
resolveFalling False = GridUpdater updateNotFalling

handleGravity :: TileMap -> (BoardState, Board a) -> (BoardState, Board a)
handleGravity m (bs, b) = unzipGrid $
                          updateColumns (resolveFalling False) $
                          updateColumnsRev (gravityUpdater m) $
                          zipGrid bs b

module TetrisAttack.Grid (
  Grid2D, GridLocation2D,
  get2D, update2D, bulkUpdate2D,
  GridWalker(..),
  walkRows, walkColumns,
  mapGrid, mapGridM, mapGridM_, unzipGrid,
  GridUpdater(..), updateColumns
) where

--------------------------------------------------------------------------------

import qualified Data.Vector as V

--------------------------------------------------------------------------------

statefulScanl :: (a -> b -> (c, a)) -> a -> V.Vector b -> V.Vector c
statefulScanl f start v = let
   stepElement [] _ = []
   stepElement (x : xs) stepper = let
     (result, next) = f stepper x
     in
      result : (stepElement xs next)
  in
   V.fromList $ stepElement (V.toList v) start

type Grid2D a = V.Vector (V.Vector a)
type GridLocation2D = (Int, Int)

mapGrid :: (a -> b) -> Grid2D a -> Grid2D b
mapGrid f = V.map (\v -> V.map f v)

mapGridM :: Monad m => (a -> m b) -> Grid2D a -> m (Grid2D b)
mapGridM f = V.mapM (\v -> V.mapM f v)

mapGridM_ :: Monad m => (a -> m b) -> Grid2D a -> m ()
mapGridM_ f g = do
  _ <- mapGridM f g
  return ()

unzipGrid :: Grid2D (b, c) -> (Grid2D b, Grid2D c)
unzipGrid = V.unzip . (V.map V.unzip)

get2D :: GridLocation2D -> Grid2D a -> a
get2D (x, y) b = (b V.! (x - 1)) V.! (y - 1)

update2D :: a -> GridLocation2D -> Grid2D a -> Grid2D a
update2D val (x, y) board = let
  col = board V.! (x - 1)
  newcol = col V.// [((y - 1), val)]
  in
   board V.// [((x - 1), newcol)]

bulkUpdate2D :: a -> [GridLocation2D] -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
bulkUpdate2D val = flip $ foldr (update2D val)

data GridWalker a b = Result b
                    | Walker (Maybe a -> GridWalker a b)

stepWalker :: GridWalker a b -> a -> GridWalker a b
stepWalker (Result b) _ = Result b
stepWalker (Walker f) x = f (Just x)

finishWalker :: GridWalker a b -> b
finishWalker (Result b) = b
finishWalker (Walker f) = finishWalker $ f Nothing

walkRows :: Grid2D a -> GridWalker a b -> [b]
walkRows grid walker
  | V.length grid == 0 = []
  | otherwise = let
    step :: V.Vector (GridWalker a b) -> V.Vector a -> V.Vector (GridWalker a b)
    step = V.zipWith stepWalker
    in
     V.toList $ V.map finishWalker $ V.foldl' step (V.map (\_ -> walker) (V.head grid)) grid

walkColumns :: Grid2D a -> GridWalker a b -> [b]
walkColumns grid walker = V.toList $ V.map (finishWalker . (V.foldl' stepWalker walker)) grid

newtype GridUpdater a b = GridUpdater { updateGridValue :: a -> (b, GridUpdater a b) }

updateColumns :: GridUpdater a b -> Grid2D a -> Grid2D b
updateColumns updater = V.map updateColumn
  where updateColumn = statefulScanl updateGridValue updater

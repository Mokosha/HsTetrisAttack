module TetrisAttack.Tile (
  TileColor(..), Tile(..), TileMap, TileLogic,
  loadTiles,
  renderTile, blank, stationary, moving, falling, stillFalling, vanishing
) where
--------------------------------------------------------------------------------
import Control.Monad.RWS.Strict hiding (when)
import Control.Wire hiding ((.))
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3
import System.FilePath

import Paths_TetrisAttack
import TetrisAttack.Constants
import TetrisAttack.Cursor
--------------------------------------------------------------------------------

data TileColor = Red | Green | Blue | Yellow | Purple
               deriving (Eq, Show, Ord, Read)

data Tile =
  -- No tile
  Blank

  -- Tile has been removed recently... will be blank soon
  | Vanishing

  -- Tile that's been recently swapped into a new location
  | Moving

  -- Recently vacated tile... will become blank soon
  | SwappedOut

  -- Stationary tile: can be swapped
  | Stationary TileColor

  -- Tile that's falling with whether or not it will inhibit on the next frame, and
  -- the distance it is away from the bottom
  | Falling Bool Float TileColor

  -- Tile that started falling... will become blank soon
  | FallingOut
  deriving (Eq, Show, Ord, Read)

type TileMap = Map.Map TileColor L.RenderObject

loadTiles :: IO (TileMap)
loadTiles = let
  tilecolors = [Red, Green, Blue, Yellow, Purple]

  loadColor :: TileColor -> IO (L.RenderObject)
  loadColor color = do
    let filename = concat ["element_", map toLower $ show color, "_square" <.> "png"]
    (Just tex) <- getDataFileName filename >>= L.loadTextureFromPNG
    L.createRenderObject L.quad (L.createTexturedMaterial tex)
  in do
    ros <- mapM loadColor tilecolors
    return $ Map.fromList $ zip tilecolors ros

type TileLogic = L.GameWire Float Tile

renderTile :: L.RenderObject -> V2 Float -> L.GameMonad ()
renderTile ro (V2 trx try) = let
  xf = L.translate (V3 trx try $ renderDepth RenderLayer'Tiles) $
       L.nonuniformScale (0.5 *^ (fmap fromIntegral (V3 blockSize blockSize 2))) $
       L.identity
  in
   censor (L.Render3DAction xf ro :) $ return ()

blank :: L.GameWire Float Tile
blank = pure Blank

stationary :: TileMap -> TileColor -> Location -> L.GameWire Float Tile
stationary m color loc = mkGen_ $ \_ -> do
  renderTile (m Map.! color) (blockCenter loc)
  return $ Right $ Stationary color

countFromOne :: Float -> L.GameWire Float Float
countFromOne end = (countToOne end) >>> (mkSF_ (1.0 -))

countToOne :: Float -> L.GameWire Float Float
countToOne end = mkPure_ $ \t ->
  if t > end then
    Left ()
  else
    Right $ t / end

timer :: Float -> L.GameWire a Float
timer duration = timeF >>> (countFromOne duration)

moving :: TileMap -> TileColor -> Location -> Location -> L.GameWire Float Tile
moving m color start end =
  let smoothstep :: L.GameWire Float Float
      smoothstep = mkSF_ $ \x -> let x3 = x*x*x in 6*x3*x*x - 15*x3*x + 10*x3

      lerpWire :: L.GameWire Float (V2 Float)
      lerpWire = mkSF_ $ \t -> lerp t (blockCenter start) (blockCenter end)

      movingWire :: L.GameWire (V2 Float) Tile
      movingWire = mkGen_ $ \pos -> do
        renderTile (m Map.! color) pos
        return $ Right Moving
  in
   (timer gSwapTime >>> smoothstep >>> lerpWire >>> movingWire) --> (stationary m color end)

falling :: TileMap -> TileColor -> Int -> Int -> L.GameWire Float Tile
falling m color col end =
  let awareTimer :: Float -> L.GameWire a (Bool, Float)
      awareTimer duration = timeF >>> (lastFrame &&& (countToOne duration >>> modTime))
        where
          modTime = mkSF_ $ \t -> (t*t*(2-t))
          lastFrame = mkSF $ \dt t -> ((duration - t) < (dtime dt), lastFrame)

      lerpWire :: L.GameWire Float (V2 Float)
      lerpWire = mkSF_ $ \t -> lerp t (blockCenter (col, end)) (blockCenter (col, end + 1))

      movingWire :: L.GameWire (Bool, V2 Float) Tile
      movingWire = mkGen_ $ \(lastFrame, pos) -> do
        let (V2 _ yoff) = pos ^-^ (blockCenter (col, end))
        renderTile (m Map.! color) pos
        return $ Right (Falling lastFrame yoff color)
  in
   ((awareTimer gTileFallTime) >>> (second lerpWire) >>> movingWire) --> (stationary m color (col, end))

stillFalling :: TileMap -> TileColor -> Float -> Int -> Int -> L.GameWire Float Tile
stillFalling m color offset col end =
  let (V2 trx try) = blockCenter (col, end)

      reduce :: Float -> L.GameWire a (Bool, Float)
      reduce 0 = empty
      reduce dist = mkPure $ \t _ ->
        let travelling = min (gTileFallSpeed * (dtime t)) dist
            remaining = dist - travelling
        in (Right (remaining < travelling, remaining), reduce remaining)

      movingWire :: L.GameWire (Bool, Float) Tile
      movingWire = mkGen_ $ \(lastFrame, yoff) -> do
        renderTile (m Map.! color) (V2 trx (try + yoff))
        return $ Right (Falling lastFrame yoff color)
  in
   (reduce (fromIntegral blockSize + offset) >>> movingWire) --> (stationary m color (col, end))

vanishing :: TileMap -> TileColor -> Location -> L.GameWire Float Tile
vanishing m color loc = let
  alpha :: L.GameWire a Float
  alpha = timer gVanishTime

  render :: L.GameWire (Float, Float) Tile
  render = mkGen_ $ \(a, _) -> let
    setAlpha ro = ro { L.material = Map.insert "alpha" (L.FloatVal a) (L.material ro),
                       L.flags = L.Transparent : (L.flags ro) }
    in do
      renderTile (setAlpha $ m Map.! color) (blockCenter loc)
      return $ Right Vanishing

  in
   ((alpha &&& mkId) >>> render) --> (blank)

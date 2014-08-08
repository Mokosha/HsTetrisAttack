module TetrisAttack.Tile (
  TileColor(..), Tile(..), TileMap, TileLogic,
  loadTiles,
  renderTile, blank, stationary, swapping, falling, stillFalling, vanishing
) where
--------------------------------------------------------------------------------
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

type TileLogic a = L.GameWire a (V2 Float -> L.GameMonad Tile)

renderTile :: L.RenderObject -> V2 Float -> L.GameMonad ()
renderTile ro (V2 trx try) = let
  xf = L.translate (V3 trx try $ renderDepth RenderLayer'Tiles) $
       L.nonuniformScale (0.5 *^ (fmap fromIntegral (V3 blockSize blockSize 2))) $
       L.identity
  in
   L.addRenderAction xf ro

blank :: TileLogic a
blank = pure (\_ -> return Blank)

stationary :: TileMap -> TileColor -> TileLogic a
stationary m color = mkSF_ $ \_ -> \pos -> do
  renderTile (m Map.! color) pos
  return $ Stationary color

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

swapping :: TileMap -> TileColor -> Bool -> TileLogic a
swapping m color movingLeft =
  let smoothstep :: L.GameWire Float Float
      smoothstep = mkSF_ $ \x -> let x3 = x*x*x in 6*x3*x*x - 15*x3*x + 10*x3

      movingWire :: L.GameWire Float (V2 Float -> L.GameMonad Tile)
      movingWire = mkSF_ $ \t -> \pos@(V2 px py) -> do
        let otherPos =
              if movingLeft
                then (V2 (px - (fromIntegral blockSize)) py)
                else (V2 (px + (fromIntegral blockSize)) py)
        renderTile (m Map.! color) (lerp t otherPos pos)
        return Moving
  in
   (timer gSwapTime >>> smoothstep >>> movingWire) --> (stationary m color)

falling :: TileMap -> TileColor -> TileLogic a
falling m color =
  let awareTimer :: Float -> L.GameWire a (Bool, Float)
      awareTimer duration = timeF >>> (lastFrame &&& (countToOne duration >>> modTime))
        where
          modTime = mkSF_ $ \t -> (t*t*(2-t))
          lastFrame = mkSF $ \dt t -> ((duration - t) < (dtime dt), lastFrame)

      movingWire :: L.GameWire (Bool, Float) (V2 Float -> L.GameMonad Tile)
      movingWire = mkSF_ $ \(lastFrame, t) -> \pos@(V2 px py) -> do
        let renderPos = lerp t pos (V2 px $ py + (fromIntegral blockSize))
            (V2 _ yoff) = renderPos ^-^ pos
        renderTile (m Map.! color) renderPos
        return $ Falling lastFrame yoff color
  in
   (awareTimer gTileFallTime >>> movingWire) --> (stationary m color)

stillFalling :: TileMap -> TileColor -> Float -> TileLogic a
stillFalling m color offset =
  let reduce :: Float -> L.GameWire a (Bool, Float)
      reduce 0 = empty
      reduce dist = mkPure $ \t _ ->
        let travelling = min (gTileFallSpeed * (dtime t)) dist
            remaining = dist - travelling
        in (Right (remaining < travelling, remaining), reduce remaining)

      movingWire :: L.GameWire (Bool, Float) (V2 Float -> L.GameMonad Tile)
      movingWire = mkSF_ $ \(lastFrame, yoff) -> \(V2 px py) -> do
        renderTile (m Map.! color) (V2 px (py + yoff))
        return $ Falling lastFrame yoff color
  in
   ((reduce $ fromIntegral blockSize + offset) >>> movingWire) --> (stationary m color)

vanishing :: Float -> TileMap -> TileColor -> TileLogic a
vanishing delayTime m color = let
  alpha :: L.GameWire a Float
  alpha = timer gVanishTime

  render :: L.GameWire Float (V2 Float -> L.GameMonad Tile)
  render = mkSF_ $ \a -> \pos -> let
    setAlpha ro = ro { L.material = Map.insert "alpha" (L.FloatVal a) (L.material ro),
                       L.flags = if a < 1.0
                                 then (L.Transparent : (L.flags ro))
                                 else (L.flags ro) }
    in do
      renderTile (setAlpha $ m Map.! color) pos
      return Vanishing

  in
   ((timer delayTime >>> pure 1.0) >>> render) -->
   (alpha >>> render) -->
   blank

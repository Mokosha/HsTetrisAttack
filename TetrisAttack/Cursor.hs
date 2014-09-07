module TetrisAttack.Cursor (
  Cursor, mkCursor
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.), id)
import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3
import System.FilePath

import FRP.Netwire.Input

import Paths_TetrisAttack
import TetrisAttack.Constants
import TetrisAttack.Grid
--------------------------------------------------------------------------------
type Cursor = (GridLocation2D, Bool)

setTrans :: L.RenderObject -> L.RenderObject
setTrans ro = ro { L.flags = L.Transparent : (L.flags ro) }

renderCursor :: L.RenderObject -> Float -> Cursor -> L.GameMonad ()
renderCursor ro yoffset ((curx, cury), _) = L.addRenderAction xf ro
  where
    bs :: Float
    bs = fromIntegral blockSize

    (V2 trx try) = 0.5 *^ (blockCenter (curx, cury) ^+^ (blockCenter (curx + 1, cury)))

    xf :: L.Transform
    xf = L.translate (V3 trx (try + yoffset) $ renderDepth RenderLayer'Cursor) $
         L.nonuniformScale (V3 (bs*8/7) (bs*4/7) 1) $
         L.identity

offsetWire :: L.GameWire Float Float
offsetWire = (when (> bs) - (pure bs)) <|> mkId where bs = fromIntegral blockSize

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd = fmap

keyW :: GLFW.Key -> (a -> a) -> L.GameWire a a
keyW key fn = (keyDebounced key >>> (arr fn)) <|> mkId

inputWire :: L.GameWire GridLocation2D Cursor
inputWire =
  (second $ keyW GLFW.Key'Up (+1)) >>>
  (second $ keyW GLFW.Key'Down (flip (-) 1)) >>>
  (first $ keyW GLFW.Key'Left (flip (-) 1)) >>>
  (first $ keyW GLFW.Key'Right (+1)) >>>
  (arr $ \(x, y) -> (L.clamp x 1 (blocksPerRow - 1), L.clamp y 1 rowsPerBoard)) &&&
  ((keyDebounced GLFW.Key'Space >>> (pure True)) <|> (pure False))

modulatePosition :: Float -> Cursor -> Cursor
modulatePosition yoff c
  | yoff > (fromIntegral blockSize) = mapFst (mapSnd (+1)) c
  | otherwise = c

mkCursor :: GridLocation2D -> IO (L.GameWire Float (L.GameMonad (), Cursor))
mkCursor loc' = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTexture
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (cursorLogic >>> (cursorRenderer $ setTrans ro))
  where
    cursorRenderer :: L.RenderObject -> L.GameWire (Float, Cursor) (L.GameMonad (), Cursor)
    cursorRenderer ro = mkSF_ $ \(yoffset, c) -> (renderCursor ro yoffset c, c)

    cursorLogic :: L.GameWire Float (Float, Cursor)
    cursorLogic = offsetWire &&& cursorWire
      where
        handleIpt :: L.GameWire Cursor Cursor
        handleIpt = (arr fst) >>> (delay loc') >>> inputWire

        cursorWire :: L.GameWire Float Cursor
        cursorWire = loop $ second handleIpt >>>
                     (arr $ \(y, x) -> let z = modulatePosition y x in (z, z))

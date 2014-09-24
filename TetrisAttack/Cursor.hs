module TetrisAttack.Cursor (
  Cursor, CursorLogic, mkCursor
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
type CursorLogic = L.GameWire Bool (L.GameMonad (), Cursor)

setTrans :: L.RenderObject -> L.RenderObject
setTrans ro = ro { L.flags = L.Transparent : (L.flags ro) }

renderCursor :: L.RenderObject -> Cursor -> L.GameMonad ()
renderCursor ro ((curx, cury), _) = L.addRenderAction xf ro
  where
    (V2 trx try) = 0.5 *^ (blockCenter (curx, cury) ^+^ (blockCenter (curx + 1, cury)))

    xf :: L.Transform
    xf = L.translate (V3 trx try $ renderDepth RenderLayer'Cursor) $
         L.nonuniformScale (V3 (blockSizeN*8/7) (blockSizeN*4/7) 1) $
         L.identity

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

modulatePosition :: Bool -> Cursor -> Cursor
modulatePosition False c = c
modulatePosition True ((x, y), p) = ((x, y+1), p)

mkCursor :: GridLocation2D -> IO (CursorLogic)
mkCursor loc' = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTexture
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (cursorLogic >>> (cursorRenderer $ setTrans ro))
  where
    cursorRenderer :: L.RenderObject -> L.GameWire Cursor (L.GameMonad (), Cursor)
    cursorRenderer ro = mkSF_ $ \c -> (renderCursor ro c, c)

    cursorLogic :: L.GameWire Bool Cursor
    cursorLogic = cursorWire
      where
        handleIpt :: L.GameWire Cursor Cursor
        handleIpt = (arr fst) >>> (delay loc') >>> inputWire

        cursorWire :: L.GameWire Bool Cursor
        cursorWire = loop $ second handleIpt >>>
                     (arr $ \(y, x) -> let z = modulatePosition y x in (z, z))

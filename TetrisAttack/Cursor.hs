module TetrisAttack.Cursor (
  Cursor, mkCursor
) where

--------------------------------------------------------------------------------
import Control.Monad.RWS.Strict hiding (when)
import Control.Wire hiding ((.), id)
import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3
import System.FilePath

import Paths_TetrisAttack
import TetrisAttack.Constants
import TetrisAttack.Grid
--------------------------------------------------------------------------------

type Cursor = (GridLocation2D, Bool)

setTrans :: L.RenderObject -> L.RenderObject
setTrans ro = ro { L.flags = L.Transparent : (L.flags ro) }

mkCursor :: GridLocation2D -> IO (L.GameWire Float Cursor)
mkCursor loc' = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTextureFromPNG
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (cursor loc' >>> (cursorRenderer $ setTrans ro))
  where
    cursorRenderer :: L.RenderObject -> L.GameWire (Float, Cursor) Cursor
    cursorRenderer ro = mkGen_ $ \(yoffset, c@((curx, cury), _)) -> do
      let bs :: Float
          bs = fromIntegral blockSize
          (V2 trx try) = 0.5 *^ (blockCenter (curx, cury) ^+^ (blockCenter (curx + 1, cury)))
          xf :: L.Transform
          xf = L.translate (V3 trx (try + yoffset) $ renderDepth RenderLayer'Cursor) $
               L.nonuniformScale (V3 (bs*8/7) (bs*4/7) 1) $
               L.identity

      L.addRenderAction xf ro
      return (Right c)
      
    cursor :: GridLocation2D -> L.GameWire Float (Float, Cursor)
    cursor l =
      arr (\y -> if y > (fromIntegral blockSize)
                 then y - (fromIntegral blockSize)
                 else y)
      &&& (cursor' l)
      where cursor' oldloc = mkGenN $ \yoff -> do
              ipt <- get
              let mapFst f (a, b) = (f a, b)
                  mapSnd = fmap
                  newloc =
                    mapSnd (\y -> if yoff > (fromIntegral blockSize) then (y + 1) else y) $
                    mapFst (\x -> L.clamp x 1 (blocksPerRow - 1)) $
                    mapSnd (\x -> L.clamp x 1 rowsPerBoard) $
                    L.withPressedKey ipt GLFW.Key'Up (mapSnd (+1)) $
                    L.withPressedKey ipt GLFW.Key'Down (mapSnd (flip (-) 1)) $
                    L.withPressedKey ipt GLFW.Key'Left (mapFst (flip (-) 1)) $
                    L.withPressedKey ipt GLFW.Key'Right (mapFst (+1)) oldloc
              put $ foldl (flip L.debounceKey) ipt
                [GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Left, GLFW.Key'Right, GLFW.Key'Space]
              return (Right (newloc, L.isKeyPressed GLFW.Key'Space ipt), cursor' newloc)


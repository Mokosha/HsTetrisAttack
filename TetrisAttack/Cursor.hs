module TetrisAttack.Cursor (
  Cursor, CursorLogic, CustomCursorLogic,
  CursorCommand(..),
  loadCursorTex, playerCursor, customCursor
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.), id)
import Data.List
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
type CustomCursorLogic a = L.GameWire (Bool, a) (L.GameMonad (), Cursor)

data CursorCommand =
  CursorCommand'MoveLeft
  | CursorCommand'MoveRight
  | CursorCommand'MoveDown
  | CursorCommand'MoveUp
  | CursorCommand'Swap
    deriving(Eq, Ord, Enum, Show)

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

clampCursor :: Cursor -> Cursor
clampCursor ((x, y), p) = ((L.clamp x 1 (blocksPerRow - 1), L.clamp y 1 rowsPerBoard), p)

commandToCursor :: Cursor -> CursorCommand -> Cursor
commandToCursor ((x, y), p) CursorCommand'MoveLeft = clampCursor ((x - 1, y), p)
commandToCursor ((x, y), p) CursorCommand'MoveRight = clampCursor ((x + 1, y), p)
commandToCursor ((x, y), p) CursorCommand'MoveDown = clampCursor ((x, y - 1), p)
commandToCursor ((x, y), p) CursorCommand'MoveUp = clampCursor ((x, y + 1), p)
commandToCursor (l, _) CursorCommand'Swap = (l, True)

commandsToCursor :: GridLocation2D -> [CursorCommand] -> Cursor
commandsToCursor (x, y) = foldl' commandToCursor ((x, y), False)

commandWire :: L.GameWire a [CursorCommand] -> L.GameWire (GridLocation2D, a) Cursor
commandWire cmd = (second cmd >>>) $ arr $ uncurry commandsToCursor

collectWires :: [L.GameWire a b] -> L.GameWire a [b]
collectWires [] = pure []
collectWires (w:ws) =
  let bs = collectWires ws
  in (w &&& bs >>> (arr $ uncurry (:))) <|> bs

inputCommands :: L.GameWire a [CursorCommand]
inputCommands = collectWires [
  keyDebounced GLFW.Key'Up >>> (pure CursorCommand'MoveUp),
  keyDebounced GLFW.Key'Down >>> (pure CursorCommand'MoveDown),
  keyDebounced GLFW.Key'Left >>> (pure CursorCommand'MoveLeft),
  keyDebounced GLFW.Key'Right >>> (pure CursorCommand'MoveRight),
  keyDebounced GLFW.Key'Space >>> (pure CursorCommand'Swap)]

inputWire :: L.GameWire GridLocation2D Cursor
inputWire = mkId &&& mkId >>> (commandWire inputCommands)

modulatePosition :: Bool -> Cursor -> Cursor
modulatePosition False c = c
modulatePosition True ((x, y), p) = ((x, y+1), p)

cursorRenderer :: L.RenderObject -> L.GameWire Cursor (L.GameMonad (), Cursor)
cursorRenderer ro = mkSF_ $ \c -> (renderCursor ro c, c)

loadCursorTex :: IO (L.RenderObject)
loadCursorTex = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTexture
  L.createRenderObject L.quad (L.createTexturedMaterial tex)

playerCursor :: L.RenderObject -> GridLocation2D -> CursorLogic
playerCursor ro loc' = cursorWire >>> (cursorRenderer $ setTrans ro)
  where
    handleIpt :: L.GameWire Cursor Cursor
    handleIpt = (arr fst) >>> (delay loc') >>> inputWire

    cursorWire :: L.GameWire Bool Cursor
    cursorWire = loop $ second handleIpt >>>
                 (arr $ \(y, x) -> let z = modulatePosition y x in (z, z))

customCursor :: L.RenderObject ->
                (GridLocation2D, a) ->
                L.GameWire a [CursorCommand] ->
                CustomCursorLogic a
customCursor ro cursorInit cmdW = cursorWire >>> (cursorRenderer $ setTrans ro)
  where
    cursorWire = loop $ second runCommands >>> (arr handleNewRow)
    runCommands = first (arr fst) >>> delay cursorInit >>> commandWire cmdW
    handleNewRow ((b, x), c) = let c' = modulatePosition b c in (c', (c', x))

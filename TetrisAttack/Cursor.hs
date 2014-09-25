module TetrisAttack.Cursor (
  Cursor, CursorLogic,
  CursorCommand(..),
  mkCursor, inputCommands
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.), id)
import Data.List
import Data.Tuple (swap)
import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3
import System.FilePath
import System.Random

import FRP.Netwire.Input

import Paths_TetrisAttack
import TetrisAttack.Constants
import TetrisAttack.Grid
--------------------------------------------------------------------------------
type Cursor = (GridLocation2D, Bool)
type CursorLogic a = L.GameWire (Bool, a) (L.GameMonad (), Cursor)

data CursorCommand =
  CursorCommand'MoveLeft
  | CursorCommand'MoveRight
  | CursorCommand'MoveDown
  | CursorCommand'MoveUp
  | CursorCommand'Swap
    deriving(Eq, Ord, Bounded, Enum, Show)

instance Random CursorCommand where
  randomR (c1, c2) gen =
    let (r, gen') = randomR (fromEnum c1, fromEnum c2) gen
    in (toEnum r, gen')

  random = randomR (minBound, maxBound)

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

commandsToCursor :: Cursor -> [CursorCommand] -> Cursor
commandsToCursor c = foldl' commandToCursor c

commandWire :: L.GameWire (Cursor, a) [CursorCommand] -> L.GameWire (Cursor, a) Cursor
commandWire cmd = ((arr fst) &&& cmd) >>> (arr $ uncurry commandsToCursor)

collectWires :: [L.GameWire a b] -> L.GameWire a [b]
collectWires [] = pure []
collectWires (w:ws) =
  let bs = collectWires ws
  in (w &&& bs >>> (arr $ uncurry (:))) <|> bs

inputCommands :: L.GameWire (Cursor, a) [CursorCommand]
inputCommands = (arr snd) >>> (collectWires [
  keyDebounced GLFW.Key'Up >>> (pure CursorCommand'MoveUp),
  keyDebounced GLFW.Key'Down >>> (pure CursorCommand'MoveDown),
  keyDebounced GLFW.Key'Left >>> (pure CursorCommand'MoveLeft),
  keyDebounced GLFW.Key'Right >>> (pure CursorCommand'MoveRight),
  keyDebounced GLFW.Key'Space >>> (pure CursorCommand'Swap)])

modulatePosition :: Bool -> Cursor -> Cursor
modulatePosition False c = c
modulatePosition True ((x, y), p) = clampCursor ((x, y+1), p)

cursorRenderer :: L.RenderObject -> L.GameWire Cursor (L.GameMonad (), Cursor)
cursorRenderer ro = mkSF_ $ \c -> (renderCursor ro c, c)

loadCursorTex :: IO (L.RenderObject)
loadCursorTex = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTexture
  L.createRenderObject L.quad (L.createTexturedMaterial tex)

cursorFeedback :: L.GameWire (Cursor, a) [CursorCommand] -> L.GameWire ((Bool, a), Cursor) (Cursor, Cursor)
cursorFeedback cmdW = ((arr $ fst . fst) &&&
                       ((arr swap) >>> (second $ arr snd) >>> (commandWire cmdW))) >>>
                      (arr $ \(b, c) -> let c'@(loc, _) = modulatePosition b c in (c', (loc, False)))

mkCursor :: GridLocation2D -> L.GameWire (Cursor, a) [CursorCommand] -> IO (CursorLogic a)
mkCursor initialPosition cursorLogic = do
  ro <- loadCursorTex
  return $ cursorLoop >>> (cursorRenderer $ setTrans ro)
  where
    cursorLoop = loop $ second (delay (initialPosition, False)) >>> (cursorFeedback cursorLogic)

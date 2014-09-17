module TetrisAttack.Board (
  module TetrisAttack.Board.Types,
  initBoard, mkBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.Random
import Control.Wire hiding ((.))
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Lambency as L
import Linear.Vector
import Linear.V3
import Linear.V2

import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile

import TetrisAttack.Board.Swap
import TetrisAttack.Board.Combos
import TetrisAttack.Board.Gravity
import TetrisAttack.Board.Types
--------------------------------------------------------------------------------

initBoard :: TileMap -> Board a
initBoard m = let
  stList :: [TileLogic a]
  stList = cycle (map (stationary m) [Red, Green, Blue, Yellow, Purple])
  in generateGrid blocksPerRow rowsPerBoard $
     \x y -> if y < (rowsPerBoard - 10) then (stList !! (x + y)) else blank

updateBoard :: TileMap -> Cursor -> BoardState -> Board a -> Board a
updateBoard m c st = (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)

addBlockRow :: TileMap -> [TileColor] -> Board a -> Board a
addBlockRow tmap row = V.zipWith V.cons (V.map (stationary tmap) (V.fromList row))

renderNewRow :: L.Sprite -> [L.Sprite] -> Float -> L.GameMonad ()
renderNewRow newRowOverlay row yoff = do
  -- First render every tile in our new row
  let vyo = (V2 0 yoff)
      depth = renderDepth RenderLayer'Tiles
  sequence_ $ zipWith
    (\s x -> L.renderSprite s tileSz depth (blockCenter (x, 0) ^+^ vyo)) row [1,2..]

  -- Then render the overlay just above the rendered tiles.
  let overlayDepth = depth + 0.001
      overlaySz = V2 (blockSize * blocksPerRow) blockSize
      overlayPos = 0.5 *^ (blockCenter (1, 0) ^+^ (blockCenter (blocksPerRow, 0))) ^+^ vyo
  L.renderSpriteWithAlpha newRowOverlay 1.0 overlaySz overlayDepth overlayPos

boardWire :: TileMap -> TileGenerator -> Board a ->
             L.GameWire (Float, Cursor) ([TileColor], BoardState)
boardWire tmap generator board = mkGen $ \timestep (yoffset, cur) -> do
  -- Define the new row that is peeking out of the bottom. If we're adding
  -- the row on this instant, then generate a new row.
  let (newRow, newGenerator) = generateTiles generator blocksPerRow
      (newgen, runlogic, yoff) =
        if yoffset > blockSizeN
        then (newGenerator, addBlockRow tmap newRow board, yoffset - blockSizeN)
        else (generator, board, yoffset)

  -- Step each tile in the board
  resGrid <- mapGridM (flip (flip stepWire timestep) (Right undefined)) runlogic

  -- Split the resulting tilestates and next wires
  let (mbTiles, newlogic) = unzipGrid resGrid

  -- If we didn't inhibit, then render the tiles and advance. Otherwise this
  -- entire wire inhibits...
  case eitherGrid mbTiles of
    Left _ -> return (Left mempty, boardWire tmap newgen board)
    Right fns -> do
      let gridPositions =
            generateGrid blocksPerRow rowsPerBoard $
            \x y -> blockCenter (x+1, y+1) ^+^ (V2 0 yoff)
      st <- mapGridM (\(pos, fn) -> fn pos) (zipGrid gridPositions fns)
      return (Right (newRow, st), boardWire tmap newgen $ updateBoard tmap cur st newlogic)

-- Board logic encompasses the entire board and most of the game mechanics.
boardLogic :: TileMap -> L.Sprite -> L.RenderObject -> CursorLogic ->
              L.GameWire (Float, Cursor) ([TileColor], BoardState) ->
              L.GameWire Float BoardState
boardLogic tmap newRowOverlay bg cursor board = let
  bgxf = L.translate (V3 halfScreenSizeXf halfScreenSizeYf $
                      renderDepth RenderLayer'Board) $
         L.nonuniformScale (V3 halfBoardSizeXf halfBoardSizeYf 1) $
         L.identity
  in mkGen $ \timestep yoffset -> do
    -- Render the background
    L.addRenderAction bgxf bg

    -- Figure out what the cursor is doing, i.e. handle user input
    (Right (curRenderFn, cur), nextCursor) <- stepWire cursor timestep (Right yoffset)

    -- Set the clip to be the board space
    L.addClipRenderAction bgxf bg

    -- Step the actual board logic with the cursor to get the result
    (boardResult, nextBoard) <- stepWire board timestep (Right (yoffset, cur))

    result <- case boardResult of
      -- If we inhibit, reset the clip and abort
      Left _ -> do
        L.resetClip
        return (Left mempty, boardLogic tmap newRowOverlay bg nextCursor nextBoard)

      -- If we produced a new row, then render it before resetting the clip
      -- and continuing.
      Right (newRow, st) -> do
        renderNewRow newRowOverlay (map (tmap Map.!) newRow) yoffset
        L.resetClip
        return (Right st, boardLogic tmap newRowOverlay bg nextCursor nextBoard)

    -- Finally render the cursor
    curRenderFn
    return result

mkBoard :: TileMap -> Board a -> IO (L.GameWire Float BoardState)
mkBoard tmap board' = do
--  (Just bgTex) <- getDataFileName ("background" <.> "png") >>= L.loadTextureFromPNG
  bgTex <- L.createSolidTexture (10, 20, 10, 255)
  bg <- L.createRenderObject L.quad (L.createTexturedMaterial bgTex)

  newRowOverlay <- L.createSolidTexture (0, 0, 0, 180) >>= L.loadStaticSpriteWithTexture
  
  cursor <- mkCursor boardCenter
  stdgen <- getStdGen
  return $
    boardLogic tmap newRowOverlay bg cursor $
    boardWire tmap (shuffleTileGen stdgen) board'

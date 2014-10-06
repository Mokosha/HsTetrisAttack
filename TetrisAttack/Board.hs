module TetrisAttack.Board (
  module TetrisAttack.Board.Types,
  initBoard, mkBoard, mkAIBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.Writer
import Control.Monad.Random
import Control.Wire hiding ((.))
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Lambency as L
import Linear.V3
import Linear.V2

import TetrisAttack.AI
import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile

import TetrisAttack.Board.Swap
import TetrisAttack.Board.Combos
import TetrisAttack.Board.Gravity
import TetrisAttack.Board.Types
--------------------------------------------------------------------------------

initBoard :: BoardState
initBoard = let
  stList :: [Tile]
  stList = cycle (map Stationary [Red, Green, Blue, Yellow, Purple])
  in generateGrid blocksPerRow rowsPerBoard $
     \x y -> if y < (rowsPerBoard - 10) then (stList !! (x + y)) else Blank

updateBoard :: TileMap -> Cursor -> BoardState -> Board a -> Board a
updateBoard m c st = (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)

addBlockRow :: TileMap -> [TileColor] -> Board a -> Board a
addBlockRow tmap row = V.zipWith V.cons (V.map (stationary tmap) (V.fromList row))

renderNewRow :: L.Sprite -> [L.Sprite] -> L.GameMonad ()
renderNewRow newRowOverlay row = do
  -- First render every tile in our new row
  let depth = renderDepth RenderLayer'Tiles
  sequence_ $ zipWith
    (\s x -> L.renderSprite s tileSz depth (blockOriginf (x, 0))) row [1,2..]

  -- Then render the overlay just above the rendered tiles.
  let overlayDepth = depth + 0.001
      overlaySz = V2 (blockSize * blocksPerRow) blockSize
      overlayPos = blockOriginf (1, 0)
  L.renderSpriteWithAlpha newRowOverlay 1.0 overlaySz overlayDepth overlayPos

boardWire :: TileMap -> TileGenerator -> Board a ->
             L.GameWire (Bool, Cursor) ([TileColor], BoardState)
boardWire tmap generator board = mkGen $ \timestep (genRow, cur) -> do
  -- Define the new row that is peeking out of the bottom. If we're adding
  -- the row on this instant, then generate a new row.
  let (newRow, newGenerator) = generateTiles generator blocksPerRow
      (newgen, runlogic) =
        if genRow
        then (newGenerator, addBlockRow tmap newRow board)
        else (generator, board)

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
            generateGrid blocksPerRow rowsPerBoard $ \x y -> blockOriginf (x+1, y+1)
      st <- mapGridM (\(pos, fn) -> fn pos) (zipGrid gridPositions fns)
      return (Right (newRow, st), boardWire tmap newgen $ updateBoard tmap cur st newlogic)

bgOffset :: L.Transform
bgOffset =
  let V2 box boy = fmap fromIntegral boardOrigin
  in L.translate (V3 box boy 0) L.identity

bgxf :: L.Transform
bgxf = L.nonuniformScale (V3 boardSizeXf boardSizeYf 1) L.identity

boardFeedback :: TileMap -> L.Sprite -> L.RenderObject -> CursorLogic BoardState ->
              L.GameWire (Bool, Cursor) ([TileColor], BoardState) ->
              L.GameWire (Float, BoardState) (BoardState, BoardState)
boardFeedback tmap overlay bg cursor board = mkGen $ \timestep (yoffset, bs) -> do
  let drawBG = L.addRenderAction (L.translate (V3 0 0 $ renderDepth RenderLayer'Board) bgxf) bg
      genRow = yoffset > blockSizeN
      yoff = L.translate (V3 0 (if genRow then yoffset - blockSizeN else yoffset) 0) L.identity

  -- First draw the background
  -- !FIXME! Should we have only one draw action actually draw both into the framebuffer
  -- and the stencil buffer?
  L.addTransformedRenderAction bgOffset $ do
      
    drawBG

    -- Figure out what the cursor is doing, i.e. handle user input
    (Right (curRenderFn, cur), nextCursor) <- stepWire cursor timestep $ Right (genRow, bs)

    -- Set the clip to be the board space 
    result <- L.addClippedRenderAction drawBG $ L.addTransformedRenderAction yoff $ do

      -- Step the actual board logic with the cursor to get the result
      (boardResult, nextBoard) <- stepWire board timestep (Right (genRow, cur))

      case boardResult of
        -- If we inhibit, abort
        Left str -> return (Left str, boardFeedback tmap overlay bg nextCursor nextBoard)

        -- If we produced a new row, then render it before resetting the clip
        -- and continuing.
        Right (newRow, st) -> do
          renderNewRow overlay $ map (tmap Map.!) newRow
          return (Right (st, st), boardFeedback tmap overlay bg nextCursor nextBoard)

    -- Finally render the cursor outside of the clipped space
    L.addTransformedRenderAction yoff curRenderFn
    return result

boardLogic :: TileMap -> L.Sprite -> L.RenderObject -> BoardState ->
           CursorLogic BoardState ->
           L.GameWire (Bool, Cursor) ([TileColor], BoardState) ->
           L.GameWire Float BoardState
boardLogic tmap newRowOverlay bg bs cursor board =
  loop $ second (delay bs) >>> (boardFeedback tmap newRowOverlay bg cursor board)

mkBoardWith :: TileMap -> L.GameWire (Cursor, BoardState) [CursorCommand] -> IO (L.GameWire Float BoardState)
mkBoardWith tmap commands = do
--  (Just bgTex) <- getDataFileName ("background" <.> "png") >>= L.loadTextureFromPNG
  bgTex <- L.createSolidTexture (10, 20, 10, 255)
  bg <- L.createRenderObject L.quad (L.createTexturedMaterial bgTex)

  newRowOverlay <- L.createSolidTexture (0, 0, 0, 180) >>= L.loadStaticSpriteWithTexture

  cursor <- mkCursor boardCenter commands
  stdgen <- getStdGen
  return $
    boardLogic tmap newRowOverlay bg initBoard cursor $
    boardWire tmap (shuffleTileGen stdgen) (boardState2Board tmap initBoard)

mkBoard :: TileMap -> IO (L.GameWire Float BoardState)
mkBoard tmap = mkBoardWith tmap inputCommands

mkAIBoard :: TileMap -> IO (L.GameWire Float BoardState)
mkAIBoard tmap = mkBoardWith tmap aiCommands

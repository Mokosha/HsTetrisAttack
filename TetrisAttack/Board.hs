{-# LANGUAGE CPP #-}
module TetrisAttack.Board (
  module TetrisAttack.Board.Types,
  BoardResources(..), loadBoardResources,
  initBoard, mkBoard, mkAIBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.Reader
#if __GLASGOW_HASKELL__ <= 708
import Control.Monad.Writer
#endif
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

data BoardVars = BoardVars {
  tileMap :: TileMap,
  overlay :: L.Sprite,
  backgroundRO :: L.RenderObject,
  boardPos :: V2 Int
}

type BoardMonad = ReaderT BoardVars L.GameMonad
type BoardWire = Wire L.TimeStep String BoardMonad

data BoardResources = BoardResources {  
  boardBGRO :: L.RenderObject,
  boardRowOverlay :: L.Sprite,
  cursorResources :: CursorResources,
  boardGen :: StdGen
}

initBoard :: BoardState
initBoard = let
  stList :: [Tile]
  stList = cycle (map Stationary [Red, Green, Blue, Yellow, Purple])
  in generateGrid blocksPerRow rowsPerBoard $
     \x y -> if y < (rowsPerBoard - 10) then (stList !! (x + y)) else Blank

updateBoard :: Cursor -> BoardState -> Board a -> BoardMonad (Board a)
updateBoard c st board = do
  vars <- ask
  let m = tileMap vars
      upd = (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)
  return $ upd board

addBlockRow :: [TileColor] -> Board a -> BoardMonad (Board a)
addBlockRow row board = do
  vars <- ask
  return $ V.zipWith V.cons (V.map (stationary $ tileMap vars) (V.fromList row)) board

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

boardWire :: TileGenerator -> Board a ->
             BoardWire (Bool, Cursor) ([TileColor], BoardState)
boardWire generator board = mkGen $ \timestep (genRow, cur) -> do
  -- Define the new row that is peeking out of the bottom. If we're adding
  -- the row on this instant, then generate a new row.
  let (newRow, newGenerator) = generateTiles generator blocksPerRow
  (newgen, runlogic) <-
    if genRow
    then do
      brd <- addBlockRow newRow board
      return $ (newGenerator, brd)
    else return (generator, board)

  -- Step each tile in the board
  resGrid <- lift $ mapGridM (flip (flip stepWire timestep) (Right undefined)) runlogic

  -- Split the resulting tilestates and next wires
  let (mbTiles, newlogic) = unzipGrid resGrid

  -- If we didn't inhibit, then render the tiles and advance. Otherwise this
  -- entire wire inhibits...
  case eitherGrid mbTiles of
    Left _ -> return (Left mempty, boardWire newgen board)
    Right fns -> do
      let gridPositions =
            generateGrid blocksPerRow rowsPerBoard $ \x y -> blockOriginf (x+1, y+1)
      st <- lift $ mapGridM (\(pos, fn) -> fn pos) (zipGrid gridPositions fns)
      newBoard <- updateBoard cur st newlogic
      return (Right (newRow, st), boardWire newgen newBoard)

offsetXform :: V2 Int -> L.Transform
offsetXform off =
  let V2 ox oy = fmap fromIntegral off
  in L.translate (V3 ox oy 0) L.identity

bgxf :: L.Transform
bgxf = L.nonuniformScale (V3 boardSizeXf boardSizeYf 1) L.identity

boardFeedback :: CursorLogic BoardState ->
              BoardWire (Bool, Cursor) ([TileColor], BoardState) ->
              BoardWire (Float, BoardState) (BoardState, BoardState)
boardFeedback cursor board = mkGen $ \timestep (yoffset, bs) -> do
  vars <- ask
  
  let depthXF = L.translate (V3 0 0 $ renderDepth RenderLayer'Board) bgxf
      drawBG = L.addRenderAction depthXF (backgroundRO vars)
      genRow = yoffset > blockSizeN
      yoff = if genRow then yoffset - blockSizeN else yoffset
      offsetXF = L.translate (V3 0 yoff 0) L.identity

  -- First draw the background
  lift $ L.addTransformedRenderAction (offsetXform $ boardPos vars) $ do

    -- !FIXME! Should we have only one draw action actually draw both into the
    -- framebuffer and the stencil buffer?
    drawBG

    -- Figure out what the cursor is doing, i.e. handle user input
    (Right (curRenderFn, cur), nextCursor) <-
      stepWire cursor timestep $ Right (genRow, bs)

    -- Set the clip to be the board space 
    result <- L.addClippedRenderAction drawBG
            $ \_ -> L.addTransformedRenderAction offsetXF
            $ do
      -- Step the actual board logic with the cursor to get the result
      let stepPrg = stepWire board timestep (Right (genRow, cur))
      (boardResult, nextBoard) <- runReaderT stepPrg vars

      case boardResult of
        -- If we inhibit, abort
        Left str -> return (Left str, boardFeedback nextCursor nextBoard)

        -- If we produced a new row, then render it before resetting the clip
        -- and continuing.
        Right (newRow, st) -> do
          renderNewRow (overlay vars) $ map (tileMap vars Map.!) newRow
          return (Right (st, st), boardFeedback nextCursor nextBoard)

    -- Finally render the cursor outside of the clipped space
    L.addTransformedRenderAction offsetXF curRenderFn
    return result

boardLogic :: BoardState -> CursorLogic BoardState ->
              BoardWire (Bool, Cursor) ([TileColor], BoardState) ->
              BoardWire Float BoardState
boardLogic bs cursor board = loop $ second (delay bs) >>> (boardFeedback cursor board)

mkBoardWith :: L.GameWire (Cursor, BoardState) [CursorCommand] ->
               BoardResources -> TileMap -> V2 Int ->
               L.GameWire Float BoardState
mkBoardWith commands res tmap pos = result boardW
  where
    newRowOverlay = boardRowOverlay res
    bg = boardBGRO res
    gen = boardGen res
    cursor = mkCursor (cursorResources res) boardCenter commands
    bvars = BoardVars tmap newRowOverlay bg pos
    boardW = boardLogic initBoard cursor $
             boardWire (shuffleTileGen gen) (boardState2Board tmap initBoard)
    result w = mkGen $ \ts f -> liftM (fmap result) $ runReaderT (stepWire w ts $ Right f) bvars

mkBoard :: BoardResources -> TileMap -> V2 Int -> L.GameWire Float BoardState
mkBoard = mkBoardWith inputCommands

mkAIBoard :: BoardResources -> TileMap -> V2 Int -> L.GameWire Float BoardState
mkAIBoard = mkBoardWith aiCommands

loadBoardResources :: CursorResources -> IO (BoardResources)
loadBoardResources curRes = do
  bgTex <- L.createSolidTexture (10, 20, 10, 255)
  bg <- L.createRenderObject L.quad (L.texturedSpriteMaterial bgTex)
  newRowOverlay <- L.createSolidTexture (0, 0, 0, 180) >>= L.loadStaticSpriteWithTexture
  stdgen <- getStdGen
  return $ BoardResources {
    boardBGRO = bg,
    boardRowOverlay = newRowOverlay,
    cursorResources = curRes,
    boardGen = stdgen
  }  

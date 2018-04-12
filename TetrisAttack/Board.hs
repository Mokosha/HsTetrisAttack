{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
module TetrisAttack.Board (
  module TetrisAttack.Board.Types,
  BoardWire, BoardResources(..), loadBoardResources,
  initBoard, mkBoard, mkAIBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.Reader
#if __GLASGOW_HASKELL__ <= 708
import Control.Monad.Writer
#endif
import Control.Monad.Random
import Control.Wire
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Lambency as L
import Linear

import TetrisAttack.AI
import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Grid
import TetrisAttack.Tile

import TetrisAttack.Board.Swap
import TetrisAttack.Board.Combos
import TetrisAttack.Board.Gravity
import TetrisAttack.Board.Types

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

data BoardResources =
  BoardResources
  { tileMap :: TileMap
  , overlay :: L.Sprite
  , backgroundRO :: L.RenderObject
  , cursorResources :: CursorResources
  , boardGen :: StdGen
  , unloadBoardResources :: IO ()
  }

type BoardWire = L.ResourceContextWire BoardResources

initBoard :: BoardState
initBoard = let
  stList :: [Tile]
  stList = cycle (map Stationary [Red, Green, Blue, Yellow, Purple])
  in generateGrid blocksPerRow rowsPerBoard $
     \x y -> if y < (rowsPerBoard - 10) then (stList !! (x + y)) else Blank

updateBoard :: BoardResources -> Cursor -> BoardState -> Board a -> Board a
updateBoard vars c st =
  let m = tileMap vars
   in (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)

addBlockRow :: BoardResources -> [TileColor] -> Board a -> Board a
addBlockRow res row =
  V.zipWith V.cons (V.map (stationary $ tileMap res) (V.fromList row))

renderNewRow :: L.Sprite -> [L.Sprite] -> L.GameMonad ()
renderNewRow newRowOverlay row = do
  -- First render every tile in our new row
  let depth = renderDepth RenderLayer'Tiles
  forM_ (row `zip` [1,2..]) $ \(s, x) ->
    L.renderSprite s tileSz depth (blockOriginf (x, 0))

  -- Then render the overlay just above the rendered tiles.
  let overlayDepth = depth + 0.001
      overlaySz = V2 (blockSize * blocksPerRow) blockSize
      overlayPos = blockOriginf (1, 0)
  L.renderSpriteWithAlpha newRowOverlay 1.0 overlaySz overlayDepth overlayPos

boardWire :: BoardWire (Bool, Cursor) ([TileColor], BoardState)
boardWire = L.withResource $ \r -> runBoard (initGenerator r) (firstBoard r) r
  where
    firstBoard = flip boardState2Board initBoard . tileMap
    initGenerator = shuffleTileGen . boardGen

    runBoard generator board vars = mkGen $ \timestep (genRow, cur) -> do
      -- Define the new row that is peeking out of the bottom. If we're adding
      -- the row on this instant, then generate a new row.
      let (newRow, newGenerator) = generateTiles generator blocksPerRow
          (newgen, runlogic) = if genRow
                               then (newGenerator, addBlockRow vars newRow board)
                               else (generator, board)

      -- Step each tile in the board and split the resulting tilestates and next wires
      (mbTiles, newlogic) <-
        unzipGrid <$> mapGridM (\w -> stepWire w timestep (Right undefined)) runlogic

      -- If we didn't inhibit, then render the tiles and advance. Otherwise this
      -- entire wire inhibits...
      case eitherGrid mbTiles of
        Left _ -> return (Left mempty, runBoard newgen board vars)
        Right fns -> do
          let gridPositions =
                generateGrid blocksPerRow rowsPerBoard $ \x y -> blockOriginf (x+1, y+1)
          st <- mapGridM (\(pos, fn) -> fn pos) (zipGrid gridPositions fns)
          let newBoard = updateBoard vars cur st newlogic
          return (Right (newRow, st), runBoard newgen newBoard vars)

offsetXform :: V2 Int -> L.Transform
offsetXform off =
  let V2 ox oy = fmap fromIntegral off
  in L.translate (V3 ox oy 0) L.identity

bgxf :: L.Transform
bgxf = L.nonuniformScale (V3 boardSizeXf boardSizeYf 1) L.identity

boardFeedback :: V2 Int
              -> CursorLogic BoardState
              -> BoardWire (Bool, Cursor) ([TileColor], BoardState)
              -> BoardWire (Float, BoardState) (BoardState, BoardState)
boardFeedback pos cursor' board =
  L.transformedResourceContext (pure $ offsetXform pos) $ proc (yoffset, bs) -> do
    gr <- genRow . drawBG -< yoffset
    yXF <- offsetXF . yoff -< (yoffset, gr)
    draw -< (yXF, (gr, bs))
  where
    depthXF = L.translate (V3 0 0 $ renderDepth RenderLayer'Board) bgxf

    -- !FIXME! Should we have only one draw action actually draw both into
    -- the framebuffer and the stencil buffer?
    drawBG :: BoardWire a a
    drawBG = L.withResource $ \r -> L.staticObject (backgroundRO r) depthXF

    cursor :: BoardWire (L.Transform, (Bool, BoardState)) Cursor
    cursor = L.transformedResourceContext (arr fst)
             $ arr snd >>> L.withSubResource cursorResources cursor'

    genRow :: BoardWire Float Bool
    genRow = arr (> blockSizeN)

    yoff :: BoardWire (Float, Bool) Float
    yoff = arr $ \(yoffset, gr) -> if gr then yoffset - blockSizeN else yoffset

    offsetXF :: BoardWire Float L.Transform
    offsetXF = arr $ \y -> L.translate (V3 0 y 0) L.identity

    draw :: BoardWire (L.Transform, (Bool, BoardState)) (BoardState, BoardState)
    draw = proc s@(xf, (gr, _)) -> do
                  cur <- cursor -< s
                  drawBoard -< (xf, cur, gr)

    drawBoard :: BoardWire (L.Transform, Cursor, Bool) (BoardState, BoardState)
    drawBoard =
      L.clippedResourceContext drawBG
      $ L.transformedResourceContext (arr $ \(x, _, _) -> x)
      $ proc (_, c, gr) -> do
        st <- drawNewRow . board -< (gr, c)
        returnA -< (st, st)

    drawNewRow :: BoardWire ([TileColor], a) a
    drawNewRow = L.withResource $ \r ->
      L.everyFrame $ \(tiles, x) -> do
        renderNewRow (overlay r) $ map (tileMap r Map.!) tiles
        return x

boardLogic :: BoardState -> V2 Int -> CursorLogic BoardState ->
              BoardWire (Bool, Cursor) ([TileColor], BoardState) ->
              BoardWire Float BoardState
boardLogic bs pos cursor board =
  loop $ second (L.liftWire $ delay bs) >>> (boardFeedback pos cursor board)

mkBoardWith :: L.GameWire (Cursor, BoardState) [CursorCommand]
            -> V2 Int
            -> BoardWire Float BoardState
mkBoardWith commands pos =
  boardLogic initBoard pos (mkCursor boardCenter commands) boardWire

mkBoard :: V2 Int -> BoardWire Float BoardState
mkBoard = mkBoardWith inputCommands

mkAIBoard :: V2 Int -> BoardWire Float BoardState
mkAIBoard = mkBoardWith aiCommands

loadBoardResources :: IO (BoardResources)
loadBoardResources = do
  curRes <- loadCursorResources
  tiles <- loadTiles
  bgTex <- L.createSolidTexture $ V4 10 20 10 255
  bg <- L.createRenderObject L.quad (L.texturedSpriteMaterial bgTex)
  overlayTex <- L.createSolidTexture $ V4 0 0 0 180
  newRowOverlay <- L.loadStaticSpriteWithTexture overlayTex
  stdgen <- getStdGen

  return $
    BoardResources
    { tileMap = tiles
    , overlay = newRowOverlay
    , backgroundRO = bg
    , cursorResources = curRes
    , boardGen = stdgen
    , unloadBoardResources = do
      L.destroyTexture bgTex
      L.unloadSprite newRowOverlay
      L.unloadRenderObject bg
      forM_ tiles L.unloadSprite
      unloadCursorResources curRes
    }

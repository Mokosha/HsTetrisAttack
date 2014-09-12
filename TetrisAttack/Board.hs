module TetrisAttack.Board (
  Board, BoardState,
  initBoard, mkBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.Random
import Control.Monad.State.Strict
import Control.Wire hiding ((.))
import Data.List (nub)
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
--------------------------------------------------------------------------------

type Board a = Grid2D (TileLogic a)
type BoardState = Grid2D Tile

initBoard :: TileMap -> Board a
initBoard m = let
  stList :: [TileLogic a]
  stList = cycle (map (stationary m) [Red, Green, Blue, Yellow, Purple])
  in generateGrid blocksPerRow rowsPerBoard $
     \x y -> if y < (rowsPerBoard - 10) then (stList !! (x + y)) else blank

swapTiles :: TileMap -> Cursor -> (BoardState, Board a) -> Board a
swapTiles _ (_, False) (_, b) = b
swapTiles m ((x, y), True) (st, board) = let

  leftPos = (x, y)
  rightPos = (x + 1, y)

  leftTile = get2D leftPos st
  rightTile = get2D rightPos st

  swappingWire = (pure (\_ -> return SwappedOut) >>> (for gSwapDelay)) --> blank

  (leftLogic, rightLogic) = case (rightTile, leftTile) of
    (Stationary rcolor, Stationary lcolor) -> (swapping m rcolor False, swapping m lcolor True)
    (Blank, Stationary lcolor) -> (swappingWire, swapping m lcolor True)
    (Stationary rcolor, Blank) -> (swapping m rcolor False, swappingWire)
    (Falling True _ rcolor, Falling True _ lcolor) ->
      (swapping m rcolor False, swapping m lcolor True)
    (Blank, Falling True _ lcolor) -> (swappingWire, swapping m lcolor True)
    (Falling True _ rcolor, Blank) -> (swapping m rcolor False, swappingWire)
    _ -> (get2D leftPos board, get2D rightPos board)
  in
   update2D leftLogic leftPos $ update2D rightLogic rightPos board

handleGravity :: TileMap -> (BoardState, Board a) -> (BoardState, Board a)
handleGravity m (bs, b) = unzipGrid (V.map handleCol $ zipGrid bs b)
  where
    newlyFalling :: TileLogic a
    newlyFalling = (pure (\_ -> return FallingOut) >>> (for gTileFallTime)) --> blank

    handleCol :: V.Vector (Tile, TileLogic a) -> V.Vector (Tile, TileLogic a)
    handleCol vec = evalState (V.generateM rowsPerBoard genCol) False
      where
        -- genCol :: Int -> State Bool (Tile, TileLogic a)
        genCol row
          -- Last row, just check if falling
          | row == (rowsPerBoard - 1) = do
            isFalling <- get
            if isFalling
              then return (FallingOut, newlyFalling)
              else return (vec V.! row)

          -- If it's not the last row, then first see what the top tile *would* be
          -- and then if we're going to fall, replace the current tile with the tile
          -- that the top tile turns into...
          | otherwise = let
        
            tileOnTop = case vec V.! (row + 1) of
              -- If the tile on top is stationary, then it will begin falling
              (Stationary c, _) ->
                Just (Falling False (fromIntegral blockSize) c, falling m c)

              -- If the tile on top is falling and about to become stationary,
              -- then it will keep falling
              (Falling True x c, _) ->
                Just (Falling False (fromIntegral blockSize + x) c, stillFalling m c x)

              -- If the tile does not meet any of the above criteria, it shouldn't
              -- fall down to the next level
              _ -> Nothing

            -- We can fall into this tile, so check if anything falls into it,
            -- and if not, then become the otherwiseTile
            checkTop :: (Tile, TileLogic a) -> State Bool (Tile, TileLogic a)
            checkTop otherwiseTile =
              case tileOnTop of
                Nothing -> do { put False; return otherwiseTile }
                Just tile -> do { put True; return tile }

           in do
              isFalling <- get
              case vec V.! row of
                -- If this tile is blank, then the tile above can fall into it.
                (Blank, _) -> checkTop (Blank, blank)

                -- If this tile is not blank, but is falling itself, then the tile
                -- above can fall into it.
                x | isFalling -> checkTop (FallingOut, newlyFalling)

                -- If this tile is not blank and is not falling, then the tile above
                -- cannot fall into it, and the tile remains stationary
                  | otherwise -> return x

type Combo = (GridLocation2D, TileColor)
handleCombos :: TileMap -> (BoardState, Board a) -> (BoardState, Board a)
handleCombos m (st, b) = (bulkUpdate2D Vanishing (map fst gatheredTiles) st,
                          foldr updateLogic b gatheredTiles)
  where
    updateLogic :: Combo -> Board a -> Board a
    updateLogic (loc, color) = update2D (vanishing m color) loc

    countWalker :: GridWalker Tile [(Int, TileColor, Int)]
    countWalker = countHelper 0 1 Blank []
      where 
        countHelper :: Int -> Int -> Tile -> [(Int, TileColor, Int)] ->
                       GridWalker Tile [(Int, TileColor, Int)]
        countHelper pos cnt tile combos = Walker walkerFn
          where
            dump :: Tile -> TileColor -> GridWalker Tile [(Int, TileColor, Int)]
            dump t c = countHelper (pos + 1) 1 t ((pos, c, cnt) : combos)

            reset :: Tile -> GridWalker Tile [(Int, TileColor, Int)]
            reset t = countHelper (pos + 1) 1 t combos

            walkerFn :: Maybe Tile -> GridWalker Tile [(Int, TileColor, Int)]
            walkerFn Nothing
              | cnt >= 3 = case tile of
                (Stationary old) -> Result $ (pos, old, cnt) : combos
                _ -> error "handleCombos -- The impossible happened"
              | otherwise = Result $ combos

            walkerFn (Just (Stationary new)) =
              case tile of
                (Stationary old)
                  | old == new -> countHelper (pos + 1) (cnt + 1) (Stationary new) combos
                  | cnt >= 3 -> dump (Stationary new) old
                  | otherwise -> reset (Stationary new)
                _ -> reset (Stationary new)

            walkerFn (Just t)
              | cnt >= 3 =
                case tile of
                  (Stationary old) -> dump t old
                  _ -> reset t
              | otherwise = reset t

    mkRowCombo :: [(Int, TileColor, Int)] -> Int -> [(Combo, Int)]
    mkRowCombo l col = map (\(row, t, n) -> (((row, col), t), n)) l

    mkColCombo :: [(Int, TileColor, Int)] -> Int -> [(Combo, Int)]
    mkColCombo l row = map (\(col, t, n) -> (((row, col), t), n)) l

    expandRows :: (Combo, Int) -> [Combo]
    expandRows (((x, y), color), num) = [((col, y), color) | col <- [(x-num+1)..x]]

    gatheredRows :: [Combo]
    gatheredRows =
      (concat $ zipWith mkRowCombo (walkRows st countWalker) [1..rowsPerBoard]) >>= expandRows

    expandCols :: (Combo, Int) -> [Combo]
    expandCols (((x, y), c), num) = [((x, row), c) | row <- [(y-num+1)..y]]

    gatheredCols :: [Combo]
    gatheredCols =
      (concat $ zipWith mkColCombo (walkColumns st countWalker) [1..blocksPerRow]) >>= expandCols

    gatheredTiles :: [Combo]
    gatheredTiles = nub $ gatheredCols ++ gatheredRows

updateBoard :: TileMap -> Cursor -> BoardState -> Board a -> Board a
updateBoard m c st = (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)

addBlockRow :: TileMap -> [TileColor] -> Board a -> Board a
addBlockRow tmap row = V.zipWith V.cons (V.map (stationary tmap) (V.fromList row))

mkBoard :: TileMap -> Board a -> IO (L.GameWire Float BoardState)
mkBoard tmap board' = do
--  (Just bgTex) <- getDataFileName ("background" <.> "png") >>= L.loadTextureFromPNG
  bgTex <- L.createSolidTexture (10, 20, 10, 255)
  bg <- L.createRenderObject L.quad (L.createTexturedMaterial bgTex)

  newRowTex <- L.createSolidTexture (0, 0, 0, 180)
  newRowOverlay <- L.createRenderObject L.quad (L.createTexturedMaterial newRowTex)
  let rowOverlay = newRowOverlay { L.flags = (L.Transparent : (L.flags newRowOverlay)) }
  
  cursor <- mkCursor boardCenter
  stdgen <- getStdGen
  return $ boardLogic rowOverlay bg cursor (boardWire (shuffleTileGen stdgen) board')
  where
    stepTileLogic :: L.TimeStep -> TileLogic a ->
                     L.GameMonad (Either String (V2 Float -> L.GameMonad Tile), TileLogic a)
    stepTileLogic ts logic = stepWire logic ts (Right undefined)

    inhibitGrid :: Monoid e => Grid2D (Either e a) -> Either e (Grid2D a)
    inhibitGrid grid
      | V.any (\v -> V.any (\x -> case x of
                               Left _ -> True
                               Right _ -> False) v) grid = Left mempty
      | otherwise = Right $ mapGrid (\(Right x) -> x) grid

    renderNewRow :: L.RenderObject -> [TileColor] -> Float -> L.GameMonad ()
    renderNewRow newRowOverlay row yoff =
      sequence_ $ zipWith
      (\c x -> do
          let tilePos = ((blockCenter (x, 0)) ^+^ (V2 0 yoff))
          L.renderSprite (tmap Map.! c) (renderDepth RenderLayer'Tiles) tilePos
          renderTileAtDepth newRowOverlay tilePos $
            renderDepth RenderLayer'Tiles + 0.001
          return ()
      ) row [1,2..]

    boardWire :: TileGenerator -> Board a -> L.GameWire (Float, Cursor) ([TileColor], BoardState)
    boardWire generator board = mkGen $ \timestep (yoffset, cur) -> do
      let (newRow, newGenerator) = generateTiles generator blocksPerRow
          genRow = yoffset > (fromIntegral blockSize)
          (newgen, runlogic) = if genRow
                               then (newGenerator, addBlockRow tmap newRow board)
                               else (generator, board)
      resGrid <- mapGridM (stepTileLogic timestep) runlogic
      let (mbTiles, newlogic) = unzipGrid resGrid
          tileRenderFns = inhibitGrid mbTiles
          gridPositions =
                generateGrid blocksPerRow rowsPerBoard $ \x y ->
                blockCenter (x+1, y+1) ^+^ (V2 0 yoff)
                where
                  yoff = if genRow then (yoffset - (fromIntegral blockSize)) else yoffset

      case tileRenderFns of
        Left _ -> return (Left mempty, boardWire generator board)
        Right fns -> do
          st <- mapGridM (\(pos, fn) -> fn pos) (zipGrid gridPositions fns)
          return (Right (newRow, st), boardWire newgen $ updateBoard tmap cur st newlogic)

    boardLogic :: L.RenderObject -> L.RenderObject -> L.GameWire Float (L.GameMonad (), Cursor) ->
                  L.GameWire (Float, Cursor) ([TileColor], BoardState) ->
                  L.GameWire Float BoardState
    boardLogic newRowOverlay bg cursor board = let
        bgxf = L.translate (V3 halfScreenSizeXf halfScreenSizeYf $
                            renderDepth RenderLayer'Board) $
               L.nonuniformScale (V3 halfBoardSizeXf halfBoardSizeYf 1) $
               L.identity
        in mkGen $ \timestep yoffset -> do
          L.addRenderAction bgxf bg
          (Right (curRenderFn, cur), nextCursor) <- stepWire cursor timestep (Right yoffset)
          L.addClipRenderAction bgxf bg
          (boardResult, nextBoard) <- stepWire board timestep (Right (yoffset, cur))
          result <- case boardResult of
            Left _ -> do
              L.resetClip
              return (Left mempty, boardLogic newRowOverlay bg nextCursor nextBoard)
            Right (newRow, st) -> do
              renderNewRow newRowOverlay newRow yoffset
              L.resetClip
              return (Right st, boardLogic newRowOverlay bg nextCursor nextBoard)
          curRenderFn
          return result

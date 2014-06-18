module TetrisAttack.Board (
  Board, BoardState,
  initBoard, mkBoard, updateBoard
) where

--------------------------------------------------------------------------------
import Control.Monad.RWS.Strict hiding (when)
import Control.Wire hiding ((.))
import Data.List (nub)
import qualified Data.Vector as V
import qualified Lambency as L
import Linear.V3

import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Tile
--------------------------------------------------------------------------------

type Board = V.Vector (V.Vector TileLogic)
type BoardState = V.Vector (V.Vector Tile)

initBoard :: TileMap -> Board
initBoard m =
  V.generate blocksPerRow $ \col ->
  V.generate rowsPerBoard $ \row ->
  if row < (rowsPerBoard - 10) then
    map (flip (stationary m) (col+1, row+1)) [Red, Green, Blue, Yellow, Purple]
    !! ((row + col) `mod` 5)
  else
    blank

get2D :: Location -> V.Vector (V.Vector a) -> a
get2D (x, y) b = (b V.! (x - 1)) V.! (y - 1)

update2D :: a -> Location -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
update2D val (x, y) board = let
  col = board V.! (x - 1)
  newcol = col V.// [((y - 1), val)]
  in
   board V.// [((x - 1), newcol)]

bulkUpdate2D :: a -> [Location] -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
bulkUpdate2D val = flip $ foldr (update2D val)

swapTiles :: TileMap -> Cursor -> (BoardState, Board) -> Board
swapTiles _ (_, False) (_, b) = b
swapTiles m ((x, y), True) (st, board) = let

  leftPos = (x, y)
  rightPos = (x + 1, y)

  leftTile = get2D leftPos st
  rightTile = get2D rightPos st

  swappingWire = (pure SwappedOut >>> (for $ gSwapDelay)) --> blank

  (leftLogic, rightLogic) = case (rightTile, leftTile) of
    (Stationary rcolor, Stationary lcolor) ->
      (moving m rcolor rightPos leftPos,
       moving m lcolor leftPos rightPos)
    (Blank, Stationary lcolor) ->
      (swappingWire,
       moving m lcolor leftPos rightPos)
    (Stationary rcolor, Blank) ->
      (moving m rcolor rightPos leftPos,
       swappingWire)
    (Falling True _ rcolor, Falling True _ lcolor) ->
      (moving m rcolor rightPos leftPos,
       moving m lcolor leftPos rightPos)
    (Blank, Falling True _ lcolor) ->
      (swappingWire,
       moving m lcolor leftPos rightPos)
    (Falling True _ rcolor, Blank) ->
      (moving m rcolor rightPos leftPos,
       swappingWire)
    _ -> (get2D leftPos board, get2D rightPos board)

  in
   update2D leftLogic leftPos $
   update2D rightLogic rightPos board

handleGravity :: TileMap -> (BoardState, Board) -> (BoardState, Board)
handleGravity m (bs, b) = (V.map handleStateCol bs, V.generate blocksPerRow handleLogicCol)
  where
    handleStateCol :: V.Vector Tile -> V.Vector Tile
    handleStateCol vec = V.fromList $ walkCol 0 False
      where
        walkCol :: Int -> Bool -> [Tile]
        walkCol row isFalling
          | row == (rowsPerBoard - 1) && isFalling = [FallingOut]
          | row == (rowsPerBoard - 1) = [vec V.! row]
          | otherwise = let topTile = case vec V.! (row + 1) of
                              (Stationary c) ->
                                Just (Falling False (fromIntegral blockSize) c)
                              (Falling True x c) ->
                                Just (Falling False (fromIntegral blockSize + x) c)
                              _ -> Nothing
                        in
                         case vec V.! row of
                           Blank -> case topTile of
                             Nothing -> Blank : (walkCol (row + 1) False)
                             Just tile -> tile : (walkCol (row + 1) True)
                           x | isFalling ->
                             case topTile of
                               Nothing -> FallingOut : (walkCol (row + 1) False)
                               Just tile -> tile : (walkCol (row + 1) True)
                             | otherwise -> x : (walkCol (row + 1) False)

    handleLogicCol :: Int -> V.Vector TileLogic
    handleLogicCol col' = V.fromList $ walkCol 1 False
      where
        col = col' + 1

        newlyFalling :: TileLogic
        newlyFalling = (pure FallingOut >>> (for $ gTileFallTime)) --> blank

        walkCol :: Int -> Bool -> [TileLogic]
        walkCol row isFalling
          | row == rowsPerBoard && isFalling = [newlyFalling]
          | row == rowsPerBoard = [get2D (col, row) b]
          | otherwise = let topLogic = case get2D (col, row + 1) bs of
                              (Stationary color) -> Just (falling m color col row)
                              (Falling True x color) -> Just (stillFalling m color x col row)
                              _ -> Nothing
                        in
                         case get2D (col, row) bs of
                           Blank -> case topLogic of
                             Nothing -> blank : (walkCol (row + 1) False)
                             Just logic -> logic : (walkCol (row + 1) True)
                           _ | isFalling ->
                             case topLogic of
                               Nothing -> newlyFalling : (walkCol (row + 1) False)
                               Just logic -> logic : (walkCol (row + 1) True)
                             | otherwise -> (get2D (col, row) b) : (walkCol (row + 1) False)

type Combo = (Location, TileColor)
handleCombos :: TileMap -> (BoardState, Board) -> (BoardState, Board)
handleCombos m (st, b) = (bulkUpdate2D Vanishing (map (fst.fst) gatheredTiles) st,
                          foldr updateLogic b gatheredTiles)
  where
    vanishingDelay :: Float
    vanishingDelay = gVanishTime * 0.5

    updateLogic :: (Combo, Float) -> Board -> Board
    updateLogic ((loc, color), delayTime) = update2D (vanishing delayTime m color loc) loc

    countRows :: Int -> [(Combo, Int)]
    countRows row = countRowsHelper 1 0 Blank
      where
        countRowsHelper :: Int -> Int -> Tile -> [(Combo, Int)]
        countRowsHelper col accum ts
          | col > blocksPerRow && accum >= 3 =
            case ts of
              (Stationary c) -> [(((blocksPerRow, row), c), accum)]
              _ -> error "This shouldn't happen"
          | col > blocksPerRow = []
          | otherwise =
            let ts' = get2D (col, row) st
                reset = countRowsHelper (col + 1) 1 ts'
                dump c = (((col - 1, row), c), accum) : reset
                continue = countRowsHelper (col + 1) (accum + 1) ts
            in case ts of
              (Stationary old) -> case ts' of
                (Stationary new)
                  | old == new -> continue
                  | accum >= 3 -> dump old
                  | otherwise -> reset
                _ | accum >= 3 -> dump old
                  | otherwise -> reset
              _ -> reset

    expandRows :: (Combo, Int) -> [(Combo, Float)]
    expandRows (((x, y), color), num) =
      zip [((col, y), color) | col <- [(x-num+1)..x]] [0.0,vanishingDelay..]

    gatheredRows :: [(Combo, Float)]
    gatheredRows = [1..rowsPerBoard] >>= countRows >>= expandRows

    countCols :: Int -> [(Combo, Int)]
    countCols col = countColsHelper 1 0 Blank
      where
        countColsHelper :: Int -> Int -> Tile -> [(Combo, Int)]
        countColsHelper row accum ts
          | row > rowsPerBoard && accum >= 3 =
            case ts of
              (Stationary c) -> [(((col, row - 1), c), accum)]
              _ -> error "This shouldn't happen"
          | row > rowsPerBoard = []
          | otherwise =
            let ts' = get2D (col, row) st
                reset = countColsHelper (row + 1) 1 ts'
                dump c = (((col, row - 1), c), accum) : reset
                continue = countColsHelper (row + 1) (accum + 1) ts
            in case ts of
              (Stationary old) -> case ts' of
                (Stationary new)
                  | old == new -> continue
                  | accum >= 3 -> dump old
                  | otherwise -> reset
                _ | accum >= 3 -> dump old
                  | otherwise -> reset
              _ -> reset

    expandCols :: (Combo, Int) -> [(Combo, Float)]
    expandCols (((x, y), c), num) =
      zip [((x, row), c) | row <- [(y-num+1)..y]] [0.0,vanishingDelay..]

    gatheredCols :: [(Combo, Float)]
    gatheredCols = [1..blocksPerRow] >>= countCols >>= expandCols

    gatheredTiles :: [(Combo, Float)]
    gatheredTiles = nub $ gatheredCols ++ gatheredRows

updateBoard :: TileMap -> Cursor -> BoardState -> Board -> Board
updateBoard m c st = (swapTiles m c) . (handleCombos m) . (handleGravity m) . ((,) st)

type ColumnCollection = Either () ([Tile], [TileLogic])
type RowCollection = Either () ([V.Vector Tile], [V.Vector TileLogic])

mkBoard :: TileMap -> Board -> IO (L.GameWire Cursor BoardState)
mkBoard tmap board' = do
--  (Just bgTex) <- getDataFileName ("background" <.> "png") >>= L.loadTextureFromPNG
  bgTex <- L.createSolidTexture (10, 20, 10, 255)
  bg <- L.createRenderObject L.quad (L.createTexturedMaterial bgTex)
  return (boardLogic board' >>> (boardRender bg))
  where
    boardRender :: L.RenderObject -> L.GameWire BoardState BoardState
    boardRender ro = mkGen_ $ \tiles -> do
      let xf = L.translate (V3 halfScreenSizeXf halfScreenSizeYf $
                            renderDepth RenderLayer'Board) $
               L.nonuniformScale (V3 halfBoardSizeXf halfBoardSizeYf 1) $
               L.identity
      censor (L.Render3DAction xf ro :) $
        return (Right tiles)

    boardLogic :: Board -> L.GameWire Cursor BoardState
    boardLogic board = let

      collectCol :: L.TimeStep -> ColumnCollection -> TileLogic -> L.GameMonad ColumnCollection
      collectCol _ (Left _) _ = return $ Left ()
      collectCol timestep (Right (ts, wires)) wire = do
        (newTile, newWire) <- stepWire wire timestep (Right 0)
        case newTile of
          Right tile -> return $ Right (tile : ts, newWire : wires)
          Left _ -> return $ Left ()

      collectRow :: L.TimeStep -> RowCollection -> (V.Vector TileLogic) ->
                    L.GameMonad RowCollection
      collectRow _ (Left _) _ = return $ Left ()
      collectRow timestep (Right (ts, logic)) col = do
        result <- V.foldM (collectCol timestep) (Right ([], [])) col
        case result of
          Right (tiles, wires) -> return $ Right (V.fromList (reverse tiles) : ts,
                                                  V.fromList (reverse wires) : logic)
          Left _ -> return $ Left ()
     in
      mkGen $ \timestep cur -> do
        result <- V.foldM (collectRow timestep) (Right ([], [])) board
        case result of
          Right (tiles, nextBoard') -> let
            st = V.fromList $ reverse tiles
            nextBoard = V.fromList $ reverse nextBoard'
            in
             return $ (Right st, boardLogic $ updateBoard tmap cur st nextBoard)
          Left _ -> return (Left (), boardLogic board)

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
import TetrisAttack.Grid
import TetrisAttack.Tile
--------------------------------------------------------------------------------

type Board = Grid2D TileLogic
type BoardState = Grid2D Tile

initBoard :: TileMap -> Board
initBoard m =
  V.generate blocksPerRow $ \col ->
  V.generate rowsPerBoard $ \row ->
  if row < (rowsPerBoard - 10) then
    map (flip (stationary m) (col+1, row+1)) [Red, Green, Blue, Yellow, Purple]
    !! ((row + col) `mod` 5)
  else
    blank

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

type Combo = (GridLocation2D, TileColor)
handleCombos :: TileMap -> (BoardState, Board) -> (BoardState, Board)
handleCombos m (st, b) = (bulkUpdate2D Vanishing (map (fst.fst) gatheredTiles) st,
                          foldr updateLogic b gatheredTiles)
  where
    vanishingDelay :: Float
    vanishingDelay = gVanishTime * 0.5

    updateLogic :: (Combo, Float) -> Board -> Board
    updateLogic ((loc, color), delayTime) = update2D (vanishing delayTime m color loc) loc

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

    expandRows :: (Combo, Int) -> [(Combo, Float)]
    expandRows (((x, y), color), num) =
      zip [((col, y), color) | col <- [(x-num+1)..x]] [0.0,vanishingDelay..]

    gatheredRows :: [(Combo, Float)]
    gatheredRows =
      (concat $ zipWith mkRowCombo (walkRows st countWalker) [1..rowsPerBoard]) >>= expandRows

    expandCols :: (Combo, Int) -> [(Combo, Float)]
    expandCols (((x, y), c), num) =
      zip [((x, row), c) | row <- [(y-num+1)..y]] [0.0,vanishingDelay..]

    gatheredCols :: [(Combo, Float)]
    gatheredCols =
      (concat $ zipWith mkColCombo (walkColumns st countWalker) [1..blocksPerRow]) >>= expandCols

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

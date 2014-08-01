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
handleGravity m (bs, b) = unzipGrid (V.imap handleCol $ zipGrid bs b)
  where
    handleCol :: Int -> V.Vector (Tile, TileLogic) -> V.Vector (Tile, TileLogic)
    handleCol col' vec = V.fromList $ walkCol 0 False
      where
        col = col' + 1
        
        newlyFalling :: TileLogic
        newlyFalling = (pure FallingOut >>> (for $ gTileFallTime)) --> blank

        walkCol :: Int -> Bool -> [(Tile, TileLogic)]
        walkCol row isFalling
          | row == (rowsPerBoard - 1) && isFalling = [(FallingOut, newlyFalling)]
          | row == (rowsPerBoard - 1) = [vec V.! row]
          | otherwise = let topTile = case vec V.! (row + 1) of
                              (Stationary c, _) ->
                                Just (Falling False (fromIntegral blockSize) c,
                                      falling m c col (row + 1))
                              (Falling True x c, _) ->
                                Just (Falling False (fromIntegral blockSize + x) c,
                                      stillFalling m c x col (row + 1))
                              _ -> Nothing
                        in
                         case vec V.! row of
                           (Blank, _) -> case topTile of
                             Nothing -> (Blank, blank) : (walkCol (row + 1) False)
                             Just tile -> tile : (walkCol (row + 1) True)
                           x | isFalling ->
                             case topTile of
                               Nothing -> (FallingOut, newlyFalling) : (walkCol (row + 1) False)
                               Just tile -> tile : (walkCol (row + 1) True)
                             | otherwise -> x : (walkCol (row + 1) False)

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
      stepTileLogic :: L.TimeStep -> Float -> TileLogic -> L.GameMonad (Either () Tile, TileLogic)
      stepTileLogic ts up logic = stepWire logic ts (Right up)

      inhibitGrid :: Grid2D (Either () Tile) -> Either () (Grid2D Tile)
      inhibitGrid grid
        | V.any (\v -> V.any (\x -> x == Left ()) v) grid = Left ()
        | otherwise = Right $ mapGrid (\(Right t) -> t) grid
      in
       mkGen $ \timestep cur -> do
         resGrid <- mapGridM (stepTileLogic timestep 0.0) board
         let (tiles', logic) = unzipGrid resGrid
             tiles = inhibitGrid tiles'
         case tiles of
           Right st -> return (Right st, boardLogic $ updateBoard tmap cur st logic)
           Left _ -> return (Left (), boardLogic board)

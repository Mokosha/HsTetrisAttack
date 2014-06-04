module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3

import Control.Wire hiding ((.))
import Control.Monad.RWS.Strict hiding (when)

import System.FilePath

import Paths_TetrisAttack
import TetrisAttack.Constants

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Word

--------------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

type Location = (Int, Int)
type Cursor = (Location, Bool)

data TileColor = Red | Green | Blue | Yellow | Purple
               deriving (Eq, Show, Ord, Read)

-- !FIXME! Right now we have to keep track of tile location because otherwise
-- we won't know when the game ends. If we actually keep track of whether or not
-- the game ends by when we need to advance the blocks then it will only need
-- to check if blocks exist in the top.
data TileState = Blank
               | Empty
               | Stationary TileColor
               | Moving TileColor
                 deriving (Eq, Show, Ord, Read)

type TileMap = Map.Map TileColor L.RenderObject

type Tile = (Location, TileState)
type TileLogic = L.GameWire Float Tile
type Board = V.Vector (V.Vector TileLogic)
type BoardState = V.Vector (V.Vector Tile)

mkCursor :: Location -> IO (L.GameWire Float Cursor)
mkCursor loc' = do
  (Just tex) <- getDataFileName ("cursor" <.> "png") >>= L.loadTextureFromPNG
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (cursor loc' >>> (cursorRenderer ro))
  where
    cursorRenderer :: L.RenderObject -> L.GameWire Cursor Cursor
    cursorRenderer ro = mkGen_ $ \c@((curx, cury), _) -> do
      let bs :: Float
          bs = fromIntegral blockSize
          (V2 trx try) = 0.5 *^ (blockCenter (curx, cury) ^+^ (blockCenter (curx + 1, cury)))
          xf :: L.Transform
          xf = L.translate (V3 trx try $ renderDepth RenderLayer'Cursor) $
               L.nonuniformScale (V3 (bs*8/7) (bs*4/7) 1) $
               L.identity

      censor (L.Render3DAction xf ro :) $
        return (Right c)
      
    cursor :: Location -> L.GameWire Float Cursor
    cursor oldloc = mkGenN $ \_ -> do
      ipt <- get
      let mapFst f (a, b) = (f a, b)
          mapSnd = fmap
          newloc =
            mapFst (\x -> L.clamp x 1 (blocksPerRow - 1)) $
            mapSnd (\x -> L.clamp x 1 rowsPerBoard) $
            L.withPressedKey ipt GLFW.Key'Up (mapSnd (+1)) $
            L.withPressedKey ipt GLFW.Key'Down (mapSnd (flip (-) 1)) $
            L.withPressedKey ipt GLFW.Key'Left (mapFst (flip (-) 1)) $
            L.withPressedKey ipt GLFW.Key'Right (mapFst (+1)) oldloc
      put $ foldl (flip L.debounceKey) ipt
        [GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Left, GLFW.Key'Right, GLFW.Key'Space]
      return (Right (newloc, L.isKeyPressed GLFW.Key'Space ipt), cursor newloc)

blank :: Location -> L.GameWire Float Tile
blank loc = pure (loc, Blank)

stationary :: TileMap -> TileColor -> Location -> L.GameWire Float Tile
stationary m color loc =
  let ro = m Map.! color
      (V2 trx try) = blockCenter loc
      xf = L.translate (V3 trx try $ renderDepth RenderLayer'Tiles) $
           L.nonuniformScale (0.5 *^ (fmap fromIntegral (V3 blockSize blockSize 1))) $
           L.identity
  in
   mkGen_ $ \_ -> censor (L.Render3DAction xf ro :) (return $ Right (loc, Stationary color))

moving :: TileMap -> TileColor -> Location -> Location -> L.GameWire Float Tile
moving m color start end =
  let ro = m Map.! color
      startv = blockCenter start
      endv = blockCenter end

      lerpWire :: L.GameWire Float (V2 Float)
      lerpWire = mkSF_ $ \t -> ((1 - t) *^ startv) + (t *^ endv)

      smoothstep :: Float -> L.GameWire a Float
      smoothstep duration =
        timeF >>>
        (mkPure_ $ \t -> if t > duration then (Left ()) else (Right (t / duration))) >>>
        (mkSF_ $ \x -> let x3 = x*x*x in 6*x3*x*x - 15*x3*x + 10*x3)

      movingWire :: L.GameWire (V2 Float) Tile
      movingWire = mkGen_ $ \(V2 trx try) -> let
        xf = L.translate (V3 trx try $ renderDepth RenderLayer'Tiles) $
             L.nonuniformScale (0.5 *^ (fmap fromIntegral (V3 blockSize blockSize 1))) $
             L.identity
        in
         censor (L.Render3DAction xf ro :) (return $ Right (end, Moving color))
  in
   (smoothstep gSwapSpeed >>> lerpWire >>> movingWire) --> (stationary m color end)

initBoard :: TileMap -> Board
initBoard m =
  V.generate blocksPerRow $ \col ->
  V.generate rowsPerBoard $ \row ->
  if row < (rowsPerBoard - 10) then
    map (flip (stationary m) (col+1, row+1)) [Red, Green, Blue, Yellow, Purple]
    !! ((row + col) `mod` 5)
  else
    blank (col+1, row+1)

analyzeTiles :: BoardState -> GameResult
analyzeTiles st = let
  checkTile ((_, y), _) =  y > rowsPerBoard
  in
   if (V.any (\vec -> V.any checkTile vec) st) then GameOver else Running

getTile :: Location -> BoardState -> Tile
getTile (x, y) b = (b V.! (x - 1)) V.! (y - 1)

getTileLogic :: Location -> Board -> TileLogic
getTileLogic (x, y) b = (b V.! (x - 1)) V.! (y - 1)

updateTileLogic :: Location -> TileLogic -> Board -> Board
updateTileLogic (x, y) logic board = let
  col = board V.! (x - 1)
  newcol = col V.// [((y - 1), logic)]
  in
   board V.// [((x - 1), newcol)]

swapTiles :: TileMap -> Cursor -> BoardState -> Board -> Board
swapTiles _ (_, False) _ b = b
swapTiles m ((x, y), True) st board = let

  leftPos = (x, y)
  rightPos = (x + 1, y)

  (_, leftTile) = getTile leftPos st
  (_, rightTile) = getTile rightPos st

  (leftLogic, rightLogic) = case (rightTile, leftTile) of
    (Stationary rcolor, Stationary lcolor) ->
      (moving m rcolor rightPos leftPos,
       moving m lcolor leftPos rightPos)
    (Blank, Stationary lcolor) ->
      ((pure (leftPos, Empty) >>> (for gSwapSpeed)) --> (blank leftPos),
       moving m lcolor leftPos rightPos)
    (Stationary rcolor, Blank) ->
      (moving m rcolor rightPos leftPos,
       (pure (rightPos, Empty) >>> (for gSwapSpeed)) --> (blank rightPos))
    _ -> (getTileLogic leftPos board, getTileLogic rightPos board)

  in
   updateTileLogic leftPos leftLogic $
   updateTileLogic rightPos rightLogic board


handleRows :: BoardState -> Board -> Board
handleRows st = removeTiles gatheredTiles
  where
    removeTiles :: [(Location, Int)] -> Board -> Board
    removeTiles [] b = b
    removeTiles (((x, y), num) : rest) b = removeTiles rest newBoard
      where
        removeRow :: Int -> V.Vector TileLogic -> V.Vector TileLogic
        removeRow locx = flip (V.//) [(y - 1, blank (locx + 1, y))]

        newBoard = b V.// [(t, removeRow t (b V.! t)) | t <- [(x-num)..(x-1)]]

    countTiles :: Int -> [(Location, Int)]
    countTiles row = countTilesHelper 1 0 Blank
      where
        countTilesHelper :: Int -> Int -> TileState -> [(Location, Int)]
        countTilesHelper col accum ts
          | col > blocksPerRow && accum >= 3 = [((blocksPerRow, row), accum)]
          | col > blocksPerRow = []
          | otherwise = let (_, ts') = getTile (col, row) st
                            reset = countTilesHelper (col + 1) 1 ts'
                            dump = ((col - 1, row), accum) : reset
                            continue = countTilesHelper (col + 1) (accum + 1) ts
                        in case ts of
                          (Stationary old) -> case ts' of
                            (Stationary new)
                              | old == new -> continue
                              | accum >= 3 -> dump
                              | otherwise -> reset
                            _ | accum >= 3 -> dump
                              | otherwise -> reset
                          _ -> reset

    gatheredTiles :: [(Location, Int)]
    gatheredTiles = do
      row <- [1..rowsPerBoard]
      result <- countTiles row
      return result

handleCols :: BoardState -> Board -> Board
handleCols st = removeTiles gatheredTiles
  where
    removeTiles :: [(Location, Int)] -> Board -> Board
    removeTiles [] b = b
    removeTiles (((x, y), num) : rest) b = removeTiles rest newBoard
      where
        removeCol :: Int -> V.Vector TileLogic -> V.Vector TileLogic
        removeCol c = flip (V.//) [(row, blank (x, row + 1)) | row <- [(y-c)..(y-1)]]

        newBoard = b V.// [(x-1, removeCol num (b V.! (x-1)))]

    countTiles :: Int -> [(Location, Int)]
    countTiles col = countTilesHelper 1 0 Blank
      where
        countTilesHelper :: Int -> Int -> TileState -> [(Location, Int)]
        countTilesHelper row accum ts
          | row > rowsPerBoard && accum >= 3 = [((col, row - 1), accum)]
          | row > rowsPerBoard = []
          | otherwise = let (_, ts') = getTile (col, row) st
                            reset = countTilesHelper (row + 1) 1 ts'
                            dump = ((col, row - 1), accum) : reset
                            continue = countTilesHelper (row + 1) (accum + 1) ts
                        in case ts of
                          (Stationary old) -> case ts' of
                            (Stationary new)
                              | old == new -> continue
                              | accum >= 3 -> dump
                              | otherwise -> reset
                            _ | accum >= 3 -> dump
                              | otherwise -> reset
                          _ -> reset

    gatheredTiles :: [(Location, Int)]
    gatheredTiles = do
      col <- [1..blocksPerRow]
      result <- countTiles col
      return result

updateBoard :: TileMap -> Cursor -> BoardState -> Board -> Board
updateBoard m c st = (swapTiles m c st) . (handleCols st) . (handleRows st)

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

data GameResult = GameOver
                | Running
                deriving (Eq, Show, Ord, Read)

game :: IO (L.GameWire GameResult GameResult)
game = let
  tilecolors = [Red, Green, Blue, Yellow, Purple]

  tile2rgb :: TileColor -> (Word8, Word8, Word8, Word8)
  tile2rgb Red = (200, 0, 0, 255)
  tile2rgb Green = (67, 128, 67, 255)
  tile2rgb Blue = (0, 0, 200, 255)
  tile2rgb Yellow = (180, 180, 0, 255)
  tile2rgb Purple = (0, 180, 180, 255)

  loadColor :: (Word8, Word8, Word8, Word8) -> IO (L.RenderObject)
  loadColor color = do
    tex <- L.createSolidTexture color
    L.createRenderObject L.quad (L.createTexturedMaterial tex)
  in
   do
     ros <- mapM (loadColor . tile2rgb) tilecolors
     let tmap = Map.fromList $ zip tilecolors ros
     board <- mkBoard tmap $ initBoard tmap
     cursor <- mkCursor boardCenter
     return $ when (== Running) >>> (pure 0) >>> cursor >>> board >>> (arr analyzeTiles)

initGame :: IO (L.Game GameResult)
initGame = do
  noLight <- L.createNoLight
  g <- game
  return $ L.Game {
    L.staticLights = [noLight],
    L.staticGeometry = [],
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = g }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Tetris Attack"
  g <- initGame
  case m of
    (Just win) -> L.run win Running g
    Nothing -> return ()
  L.destroyWindow m

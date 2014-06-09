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
import Data.List (nub)
import Data.Char (toLower)

--------------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

type Location = (Int, Int)
type Cursor = (Location, Bool)

data TileColor = Red | Green | Blue | Yellow | Purple
               deriving (Eq, Show, Ord, Read)

data Tile =
  -- No tile
  Blank

  -- Tile that's been recently swapped into a new location
  | Moving

  -- Recently vacated tile... will become blank soon
  | SwappedOut

  -- Stationary tile: can be swapped
  | Stationary TileColor

  -- Tile that's falling with whether or not it will inhibit on the next frame, and
  -- the distance it is away from the bottom
  | Falling Bool Float TileColor

  -- Tile that started falling... will become blank soon
  | FallingOut
  deriving (Eq, Show, Ord, Read)

type TileMap = Map.Map TileColor L.RenderObject
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

renderTile :: L.RenderObject -> V2 Float -> L.GameMonad ()
renderTile ro (V2 trx try) = let
  xf = L.translate (V3 trx try $ renderDepth RenderLayer'Tiles) $
       L.nonuniformScale (0.5 *^ (fmap fromIntegral (V3 blockSize blockSize 2))) $
       L.identity
  in
   censor (L.Render3DAction xf ro :) $ return ()

blank :: L.GameWire Float Tile
blank = pure Blank

stationary :: TileMap -> TileColor -> Location -> L.GameWire Float Tile
stationary m color loc = mkGen_ $ \_ -> do
  renderTile (m Map.! color) (blockCenter loc)
  return $ Right $ Stationary color

countFromOne :: Float -> L.GameWire Float Float
countFromOne end = mkPure_ $ \t ->
  if t > end then
    Left ()
  else
    Right $ 1.0 - (t / end)

countToOne :: Float -> L.GameWire Float Float
countToOne end = (countFromOne end) >>> (mkSF_ (1.0 -))

timer :: Float -> L.GameWire a Float
timer duration = timeF >>> (countFromOne duration)

moving :: TileMap -> TileColor -> Location -> Location -> L.GameWire Float Tile
moving m color start end =
  let smoothstep :: L.GameWire Float Float
      smoothstep = mkSF_ $ \x -> let x3 = x*x*x in 6*x3*x*x - 15*x3*x + 10*x3

      lerpWire :: L.GameWire Float (V2 Float)
      lerpWire = mkSF_ $ \t -> lerp t (blockCenter start) (blockCenter end)

      movingWire :: L.GameWire (V2 Float) Tile
      movingWire = mkGen_ $ \pos -> do
        renderTile (m Map.! color) pos
        return $ Right Moving
  in
   (timer gSwapTime >>> smoothstep >>> lerpWire >>> movingWire) --> (stationary m color end)

falling :: TileMap -> TileColor -> Int -> Int -> L.GameWire Float Tile
falling m color col end =
  let awareTimer :: Float -> L.GameWire a (Bool, Float)
      awareTimer duration = timeF >>> (lastFrame &&& (countToOne duration >>> modTime))
        where
          modTime = mkSF_ $ \t -> (t*t*(2-t))
          lastFrame = mkSF $ \dt t -> ((duration - t) < (dtime dt), lastFrame)

      lerpWire :: L.GameWire Float (V2 Float)
      lerpWire = mkSF_ $ \t -> lerp t (blockCenter (col, end)) (blockCenter (col, end + 1))

      movingWire :: L.GameWire (Bool, V2 Float) Tile
      movingWire = mkGen_ $ \(lastFrame, pos) -> do
        let (V2 _ yoff) = pos ^-^ (blockCenter (col, end))
        renderTile (m Map.! color) pos
        return $ Right (Falling lastFrame yoff color)
  in
   ((awareTimer gTileFallTime) >>> (second lerpWire) >>> movingWire) --> (stationary m color (col, end))

stillFalling :: TileMap -> TileColor -> Float -> Int -> Int -> L.GameWire Float Tile
stillFalling m color offset col end =
  let (V2 trx try) = blockCenter (col, end)

      reduce :: Float -> L.GameWire a (Bool, Float)
      reduce 0 = empty
      reduce dist = mkPure $ \t _ ->
        let travelling = min (gTileFallSpeed * (dtime t)) dist
            remaining = dist - travelling
        in (Right (remaining < travelling, remaining), reduce remaining)

      movingWire :: L.GameWire (Bool, Float) Tile
      movingWire = mkGen_ $ \(lastFrame, yoff) -> do
        renderTile (m Map.! color) (V2 trx (try + yoff))
        return $ Right (Falling lastFrame yoff color)
  in
   (reduce (fromIntegral blockSize + offset) >>> movingWire) --> (stationary m color (col, end))

initBoard :: TileMap -> Board
initBoard m =
  V.generate blocksPerRow $ \col ->
  V.generate rowsPerBoard $ \row ->
  if row < (rowsPerBoard - 10) then
    map (flip (stationary m) (col+1, row+1)) [Red, Green, Blue, Yellow, Purple]
    !! ((row + col) `mod` 5)
  else
    blank

analyzeTiles :: BoardState -> GameResult
analyzeTiles st
  | V.any (\v -> v V.! (rowsPerBoard - 1) /= Blank) st = GameOver
  | otherwise = Running

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

  (leftLogic, rightLogic) = case (rightTile, leftTile) of
    (Stationary rcolor, Stationary lcolor) ->
      (moving m rcolor rightPos leftPos,
       moving m lcolor leftPos rightPos)
    (Blank, Stationary lcolor) ->
      ((pure SwappedOut >>> (for gSwapTime)) --> blank,
       moving m lcolor leftPos rightPos)
    (Stationary rcolor, Blank) ->
      (moving m rcolor rightPos leftPos,
       ((pure SwappedOut) >>> (for gSwapTime)) --> blank)
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
                              (Stationary c) -> Just (Falling False (fromIntegral blockSize) c)
                              (Falling True x c) -> Just (Falling False (fromIntegral blockSize + x) c)
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

handleCombos :: (BoardState, Board) -> (BoardState, Board)
handleCombos (st, b) = (bulkUpdate2D Blank gatheredTiles st,
                        bulkUpdate2D blank gatheredTiles b)
  where
    countRows :: Int -> [(Location, Int)]
    countRows row = countRowsHelper 1 0 Blank
      where
        countRowsHelper :: Int -> Int -> Tile -> [(Location, Int)]
        countRowsHelper col accum ts
          | col > blocksPerRow && accum >= 3 = [((blocksPerRow, row), accum)]
          | col > blocksPerRow = []
          | otherwise =
            let ts' = get2D (col, row) st
                reset = countRowsHelper (col + 1) 1 ts'
                dump = ((col - 1, row), accum) : reset
                continue = countRowsHelper (col + 1) (accum + 1) ts
            in case ts of
              (Stationary old) -> case ts' of
                (Stationary new)
                  | old == new -> continue
                  | accum >= 3 -> dump
                  | otherwise -> reset
                _ | accum >= 3 -> dump
                  | otherwise -> reset
              _ -> reset

    expandRows :: (Location, Int) -> [Location]
    expandRows ((x, y), num) = [(col, y) | col <- [(x-num+1)..x]]

    gatheredRows :: [Location]
    gatheredRows = [1..rowsPerBoard] >>= countRows >>= expandRows

    countCols :: Int -> [(Location, Int)]
    countCols col = countColsHelper 1 0 Blank
      where
        countColsHelper :: Int -> Int -> Tile -> [(Location, Int)]
        countColsHelper row accum ts
          | row > rowsPerBoard && accum >= 3 = [((col, row - 1), accum)]
          | row > rowsPerBoard = []
          | otherwise =
            let ts' = get2D (col, row) st
                reset = countColsHelper (row + 1) 1 ts'
                dump = ((col, row - 1), accum) : reset
                continue = countColsHelper (row + 1) (accum + 1) ts
            in case ts of
              (Stationary old) -> case ts' of
                (Stationary new)
                  | old == new -> continue
                  | accum >= 3 -> dump
                  | otherwise -> reset
                _ | accum >= 3 -> dump
                  | otherwise -> reset
              _ -> reset

    expandCols :: (Location, Int) -> [Location]
    expandCols ((x, y), num) = [(x, row) | row <- [(y-num+1)..y]]

    gatheredCols :: [Location]
    gatheredCols = [1..blocksPerRow] >>= countCols >>= expandCols

    gatheredTiles :: [Location]
    gatheredTiles = nub $ gatheredCols ++ gatheredRows

updateBoard :: TileMap -> Cursor -> BoardState -> Board -> Board
updateBoard m c st = (swapTiles m c) . handleCombos . (handleGravity m) . ((,) st)

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

  loadColor :: TileColor -> IO (L.RenderObject)
  loadColor color = do
    let filename = concat ["element_", map toLower $ show color, "_square" <.> "png"]
    (Just tex) <- getDataFileName filename >>= L.loadTextureFromPNG
    L.createRenderObject L.quad (L.createTexturedMaterial tex)
  in
   do
     ros <- mapM loadColor tilecolors
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

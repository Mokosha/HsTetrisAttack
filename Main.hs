module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Linear.V3

import Control.Wire
import Control.Monad.RWS.Strict hiding (when)

import System.FilePath

import Paths_TetrisAttack
import TetrisAttack.Constants

--------------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

type Location = (Int, Int)
type Cursor = (Location, Bool)

data TileColor = Red | Green | Blue | Yellow | Purple
               deriving (Eq, Show, Ord, Read)
data TileType = Stationary TileColor
                -- Tile is swapping with another, this contains what the tile will be
                -- once it's done swapping
              | Swapping Tile

type Tile = (Location, TileType)
type TileLogic = L.GameWire Tile Tile

data GameResult = GameOver
                | Running
                deriving (Eq, Show, Ord, Read)

initialTiles :: [Tile]
initialTiles = zipWith (,) ([(x, y) | x <- [1..blocksPerRow], y <- [1..(rowsPerBoard - 10)]])
               (cycle $ map Stationary [Red , Green , Blue , Yellow , Purple])

analyzeTiles :: [Tile] -> GameResult
analyzeTiles tiles 
  | any (\((_, y), _) -> y > rowsPerBoard) tiles = GameOver
  | otherwise = Running

mkCursor :: Location -> IO (L.GameWire a Cursor)
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
          xf = L.translate ((V3 trx try 0) ^-^ ((bs * 0.125) *^ (V3 1 1 0))) $
               L.translate (V3 0 0 $ renderDepth RenderLayer'Cursor) $
               L.nonuniformScale (V3 (2*bs*8/7) (bs*8/7) 1) $
               L.identity

      censor (L.Render3DAction xf ro :) $
        return (Right c)
      
    cursor :: Location -> L.GameWire a Cursor
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
        [GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Left, GLFW.Key'Right]
      return (Right (newloc, L.isKeyPressed GLFW.Key'Space ipt), cursor newloc)

tile :: Tile -> TileLogic
tile = pure

mkBoard :: [Tile] -> IO (L.GameWire Cursor [Tile])
mkBoard tiles' = do
--  (Just bgTex) <- getDataFileName ("background" <.> "png") >>= L.loadTextureFromPNG
  bgTex <- L.createSolidTexture (67, 128, 67, 255)
  bg <- L.createRenderObject L.quad (L.createTexturedMaterial bgTex)
  return (boardLogic tiles' >>> (boardRender bg))
  where
    boardRender :: L.RenderObject -> L.GameWire [Tile] [Tile]
    boardRender ro = mkGen_ $ \tiles -> do
      let xf = L.translate (V3 halfScreenSizeXf halfScreenSizeYf $ renderDepth RenderLayer'Board) $
               L.nonuniformScale (V3 halfBoardSizeXf halfBoardSizeYf 1) $
               L.identity
      censor (L.Render3DAction xf ro :) $
        return (Right tiles)

    boardLogic :: [Tile] -> L.GameWire Cursor [Tile]
    boardLogic tiles = mkGenN $ \(loc, swap) -> do
      return (Right tiles, boardLogic tiles)

game :: IO (L.GameWire GameResult GameResult)
game = do
  board <- mkBoard initialTiles
  cursor <- mkCursor boardCenter
  return $ when (== Running) >>> cursor >>> board >>> (arr analyzeTiles)

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

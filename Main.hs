module Main (main) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))
import qualified Data.Vector as V
import qualified Lambency as L
import Linear.Vector

import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Tile
import TetrisAttack.Board
--------------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

data GameResult = GameOver
                | Running
                deriving (Eq, Show, Ord, Read)

analyzeTiles :: BoardState -> GameResult
analyzeTiles st
  | V.any (\v -> v V.! (rowsPerBoard - 1) /= Blank) st = GameOver
  | otherwise = Running

game :: IO (L.GameWire GameResult GameResult)
game = do
  tmap <- loadTiles
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

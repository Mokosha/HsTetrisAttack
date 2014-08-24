module Main (main) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear.Vector

import TetrisAttack.Constants
import TetrisAttack.Game
--------------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

initGame :: IO (L.Game GameResult)
initGame = do
  noLight <- L.createNoLight
  g <- mkGame
  return $ L.Game {
    L.staticLights = [noLight],
    L.staticGeometry = [],
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = g >>> (L.quitWire GLFW.Key'Q) }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Tetris Attack"
  g <- initGame
  case m of
    (Just win) -> L.run win Running g
    Nothing -> return ()
  L.destroyWindow m

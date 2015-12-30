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
  g <- mkGame
  return $ L.Game {
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = g >>> (L.quitWire GLFW.Key'Q) }

main :: IO ()
main = L.withWindow screenSizeX screenSizeY "Tetris Attack" $
       L.loadAndRun Running initGame

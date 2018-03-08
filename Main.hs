module Main (main) where

--------------------------------------------------------------------------------
import Control.Wire

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear.Vector

import TetrisAttack.Constants
import TetrisAttack.Game

import Prelude hiding (id, (.))
--------------------------------------------------------------------------------

camera :: L.ContWire a L.Camera
camera = L.mk2DCam screenSizeX screenSizeY . pure zero

initGame :: IO (L.Game GameResult)
initGame = do
  g <- mkGame
  return $ L.Game {
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = (g >>> L.quitWire GLFW.Key'Q >>> arr Just)
                  `L.withDefault` pure Nothing }

main :: IO ()
main = L.withWindow screenSizeX screenSizeY "Tetris Attack" $
       L.loadAndRun Running initGame

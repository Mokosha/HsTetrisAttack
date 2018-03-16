module Main (main) where

--------------------------------------------------------------------------------
import Control.Wire

import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear.Vector

import TetrisAttack.Constants
import TetrisAttack.Game

import Prelude hiding (id, (.))
--------------------------------------------------------------------------------

camera :: L.ContWire a L.Camera
camera = L.mk2DCam screenSizeX screenSizeY . pure zero

tetrisAttack :: L.Game GameResult
tetrisAttack = L.Game camera [] ((id &&& quitWire) >>> game)
  where
    quitWire = (keyPressed GLFW.Key'Q >>> pure True) `L.withDefault` pure False

main :: IO ()
main = L.withWindow screenSizeX screenSizeY "Tetris Attack" $
       L.run Running tetrisAttack

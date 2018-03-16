{-# LANGUAGE Arrows #-}
module TetrisAttack.Game (
  GameResult(..),
  game
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((<>))
import qualified Data.Vector as V

import qualified Lambency as L

import Linear.Vector
import Linear.V2

import Prelude hiding (id, (.))

import TetrisAttack.Constants
import TetrisAttack.Tile
import TetrisAttack.Board
--------------------------------------------------------------------------------

type Player = BoardWire Float BoardState

data GameResult = GameOver
                | Running
                deriving (Eq, Show, Ord, Read, Enum)

instance Monoid GameResult where
  mempty = Running
  GameOver `mappend` _ = GameOver
  _ `mappend` GameOver = GameOver
  _ `mappend` _ = Running

analyzeTiles :: BoardState -> GameResult
analyzeTiles st
  | V.any (\v -> v V.! (rowsPerBoard - 1) /= Blank) st = GameOver
  | otherwise = Running

gameLoop :: Player -> BoardWire GameResult BoardState
gameLoop player =
  id &&& L.liftWire (pure 10 >>> offsetWire) >>> (
    proc (gr, yoff) -> case gr of
      GameOver -> L.liftWire mkEmpty -< ()
      Running -> player -< yoff)
  where
    offsetWire :: L.GameWire Float Float
    offsetWire = loop $ second (delay 0) >>> offsetFeedback

    offsetFeedback :: L.GameWire (Float, Float) (Float, Float)
    offsetFeedback = mkSF $ \dt (s, y) ->
      let y' = y + s*(dtime dt)
       in ((y', if y' > blockSizeN then (y' - blockSizeN) else y'),
           offsetFeedback)

twoPGameLoop :: Player
             -> Player
             -> BoardWire GameResult GameResult
twoPGameLoop p1 p2 = (p1w &&& p2w) >>> analyze >>> arr (uncurry mappend)
  where
    analyze = (arr analyzeTiles) *** (arr analyzeTiles)
    p1w = gameLoop p1
    p2w = gameLoop p2
   
game :: L.ContWire (GameResult, Bool) (Maybe GameResult)
game = L.bracketResource loadBoardResources unloadBoardResources $
  let playerBoard = mkBoard $ boardOrigin ^-^ (V2 300 0)
      aiBoard = mkAIBoard $ boardOrigin ^+^ (V2 300 0)
  in twoPGameLoop playerBoard aiBoard . L.liftWire (when (== Running))

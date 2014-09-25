module TetrisAttack.Game (
  GameResult(..),
  mkGame
) where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Control.Wire hiding ((.))
import qualified Data.Vector as V

import qualified Lambency as L

import TetrisAttack.Constants
import TetrisAttack.Tile
import TetrisAttack.Board
--------------------------------------------------------------------------------

data GameResult = GameOver
                | Running
                deriving (Eq, Show, Ord, Read)

analyzeTiles :: BoardState -> GameResult
analyzeTiles st
  | V.any (\v -> v V.! (rowsPerBoard - 1) /= Blank) st = GameOver
  | otherwise = Running

delayedLoop :: (MonadFix m, Monoid s, Monoid e) =>
               c -> Wire s e m (a, c) (b, c) -> Wire s e m a b
delayedLoop initialValue loopWire = loop $ second (delay initialValue) >>> loopWire

gameLoop :: L.GameWire Float BoardState -> L.GameWire (GameResult, Float) (BoardState, Float)
gameLoop firstBoard = let
  runBoard :: GameResult -> Float -> L.TimeStep -> L.GameWire Float BoardState ->
              L.GameMonad (Either String (BoardState, Float),
                           L.GameWire (GameResult, Float) (BoardState, Float))
  runBoard GameOver _ _ b = return (Left mempty, loopFn b)
  runBoard Running flt ts board = do
    let blockSzF = (fromIntegral blockSize)
        nextFlt = if flt > blockSzF then (flt - blockSzF) else (flt + 10*(dtime ts))
    (boardState, nextBoard) <- stepWire board ts (Right flt)
    case boardState of
      Left exception -> return (Left exception, loopFn nextBoard)
      Right bs -> return (Right (bs, nextFlt), loopFn nextBoard)

  -- !FIXME! The rate at which the board rises should be dynamic
  loopFn :: L.GameWire Float BoardState -> L.GameWire (GameResult, Float) (BoardState, Float)
  loopFn board = mkGen $ \ts (result, flt) -> runBoard result flt ts board
  in
   loopFn firstBoard
   
mkGame :: IO (L.GameWire GameResult GameResult)
mkGame = do
  board <- loadTiles >>= mkBoard
  return $ when (== Running) >>> (delayedLoop 0 $ gameLoop board) >>> (arr analyzeTiles)

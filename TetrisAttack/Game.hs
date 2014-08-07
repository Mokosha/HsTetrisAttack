module TetrisAttack.Game (
  GameResult(..),
  mkGame
) where

--------------------------------------------------------------------------------
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

semiLoop :: (Monad m, Monoid s, Monoid e) => c -> Wire s e m (a, c) (b, c) -> Wire s e m a b
semiLoop initialValue loopWire = let
  runLoop :: (Monad m, Monoid s, Monoid e) => Wire s e m (a, c) (b, c) -> s -> a -> c -> m (Either e b, Wire s e m a b)
  runLoop wire ts ipt x = do
    (result, nextWire) <- stepWire wire ts (Right (ipt, x))
    case result of
      Left i -> return (Left i, mkEmpty)
      Right (value, nextX) -> return (Right value, mkGen $ \ts' ipt' -> runLoop nextWire ts' ipt' nextX)
  in
   mkGen $ \ts input -> runLoop loopWire ts input initialValue

gameLoop :: L.GameWire Float BoardState -> L.GameWire (GameResult, Float) (BoardState, Float)
gameLoop firstBoard = let
  runBoard :: GameResult -> Float -> L.TimeStep -> L.GameWire Float BoardState ->
              L.GameMonad (Either () (BoardState, Float), L.GameWire (GameResult, Float) (BoardState, Float))
  runBoard GameOver _ _ b = return (Left (), loopFn b)
  runBoard Running flt ts board = do
    (boardState, nextBoard) <- stepWire board ts (Right flt)
    case boardState of
      Left () -> return (Left (), loopFn nextBoard)
      Right bs -> return (Right (bs, flt + (dtime ts)), loopFn nextBoard)
  
  loopFn :: L.GameWire Float BoardState -> L.GameWire (GameResult, Float) (BoardState, Float)
  loopFn board = mkGen $ \ts (result, flt) -> runBoard result flt ts board
  in
   mkGen $ \ts (result, _) -> runBoard result 0 ts firstBoard
   
mkGame :: IO (L.GameWire GameResult GameResult)
mkGame = do
  tmap <- loadTiles
  board <- mkBoard tmap $ initBoard tmap
  return $ when (== Running) >>> (loop $ gameLoop board) >>> (arr analyzeTiles)


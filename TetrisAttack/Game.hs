module TetrisAttack.Game (
  GameResult(..),
  mkGame
) where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.), (<>))
import Data.Monoid
import qualified Data.Vector as V

import qualified Lambency as L

import Linear.Vector
import Linear.V2

import TetrisAttack.Constants
import TetrisAttack.Cursor
import TetrisAttack.Tile
import TetrisAttack.Board
--------------------------------------------------------------------------------

type Player = L.GameWire Float BoardState

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

gameLoop :: L.GameWire GameResult Float -> Player -> L.GameWire GameResult BoardState
gameLoop adv player' = mkId &&& (adv >>> delay 0) >>> (loopFn 0 player')
  where
    loopFn :: Float -> Player -> L.GameWire (GameResult, Float) BoardState
    loopFn flt player = mkGen $ \ts (result, df) -> runBoard result (df + flt) ts player

    runBoard :: GameResult -> Float -> L.TimeStep -> Player ->
                L.GameMonad (Either String BoardState,
                             L.GameWire (GameResult, Float) BoardState)
    runBoard GameOver _ _ b = return (Left mempty, loopFn 0 b)
    runBoard Running flt ts board = do
      let nextFlt = if flt > blockSizeN then (flt - blockSizeN) else flt
      (boardState, nextBoard) <- stepWire board ts (Right flt)
      return (boardState, loopFn nextFlt nextBoard)

constSpeed :: L.GameWire a Float
constSpeed = mkSF $ \ts _ -> (10*(dtime ts), constSpeed)

twoPGameLoop :: L.GameWire GameResult Float -> Player -> Player ->
                L.GameWire GameResult GameResult
twoPGameLoop adv p1 p2 = runGame p1w p2w
  where
    p1w = gameLoop adv p1
    p2w = gameLoop adv p2

    runGame :: L.GameWire GameResult BoardState ->
               L.GameWire GameResult BoardState ->
               L.GameWire GameResult GameResult
    runGame w1 w2 = mkGen $ \ts gr -> do
      (r1, w1') <- stepWire w1 ts (Right gr)
      (r2, w2') <- stepWire w2 ts (Right gr)
      return $ case (r1, r2) of
        (Right bs1, Right bs2) -> (Right $ analyzeTiles bs1 <> analyzeTiles bs2, runGame w1' w2')
        (Left e1, Left e2) -> (Left (e1 `mappend` e2), runGame w1' w2')
        (Left e1, _) -> (Left e1, runGame w1' w2')
        (_, Left e2) -> (Left e2, runGame w1' w2')
   
mkGame :: IO (L.GameWire GameResult GameResult)
mkGame = do
  curRes <- loadCursorResources
  boardRes <- loadBoardResources curRes
  tiles <- loadTiles

  let playerBoard = mkBoard boardRes tiles $ boardOrigin ^-^ (V2 300 0)
      aiBoard = mkAIBoard boardRes tiles $ boardOrigin ^+^ (V2 300 0)

  return $ when (== Running) >>> (twoPGameLoop constSpeed playerBoard aiBoard)

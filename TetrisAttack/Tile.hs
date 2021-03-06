{-# LANGUAGE DeriveGeneric #-}
module TetrisAttack.Tile (
  TileColor(..), Tile(..), TileMap, TileLogic,
  loadTiles,
  TileGenerator(..), genTileGen, shuffleTileGen,
  blank, stationary, swapping, falling, stillFalling, vanishing
) where
--------------------------------------------------------------------------------
import Control.Monad.Random hiding (when)
import Control.Monad.Writer hiding (when)
import Control.Wire
import Data.Char (toLower)
import qualified Data.Map as Map
import GHC.Generics
import qualified Lambency as L
import Linear.Vector
import Linear.V2
import Prelude hiding (id, (.))
import System.FilePath

import Paths_TetrisAttack
import TetrisAttack.Constants
--------------------------------------------------------------------------------

data TileColor = Red | Green | Blue | Yellow | Purple
               deriving (Eq, Show, Ord, Read, Bounded, Enum, Generic)

data Tile =
  -- No tile
  Blank

  -- Tile has been removed recently... will be Vanished soon
  | Vanishing

  -- Tile has been removed but the empty space can be swapped... will be Blank soon
  | Vanished

  -- Tile that's been recently swapped into a new location
  | Moving TileColor

  -- Recently vacated tile... will become blank soon
  | SwappedOut

  -- Stationary tile: can be swapped
  | Stationary TileColor

  -- Tile that's falling with whether or not it will inhibit on the next frame, and
  -- the distance it is away from the bottom
  | Falling Bool Float TileColor

  -- Tile that started falling... will become blank soon
  | FallingOut
  deriving (Eq, Show, Ord, Read, Generic)

type TileMap = Map.Map TileColor L.Sprite

tilecolors :: [TileColor]
tilecolors = [minBound..maxBound]

tileDepth :: Float
tileDepth = renderDepth RenderLayer'Tiles

kNumTileColors :: Int
kNumTileColors = length tilecolors

tileintmap :: Map.Map Int TileColor
tileintmap = Map.fromList $ zip [1,2..] tilecolors

loadTiles :: L.ResourceLoader TileMap
loadTiles = let
  loadColor :: TileColor -> L.ResourceLoader L.Sprite
  loadColor color = do
    let filename = concat ["element_", toLower <$> show color, "_square" <.> "png"]
    (Just sprite) <- liftIO (getDataFileName filename) >>= L.loadStaticSprite
    return sprite
  in (Map.fromList . zip tilecolors) <$> mapM loadColor tilecolors

newtype TileGenerator = TileGen {
  generateTiles :: Int -> ([TileColor], TileGenerator)
}

genTileGen :: RandomGen g => g -> TileGenerator
genTileGen firstRand = TileGen $ genHelper firstRand
  where
    genTiles :: RandomGen g => Int -> Rand g [TileColor]
    genTiles 0 = return []
    genTiles n = do
      r <- getRandomR (1, kNumTileColors)
      moreTiles <- genTiles $ n - 1
      return $ (tileintmap Map.! r) : moreTiles

    genHelper :: RandomGen g => g -> Int -> ([TileColor], TileGenerator)
    genHelper rand 0 = ([], TileGen $ genHelper rand)
    genHelper rand n = (result, TileGen $ genHelper newRand)
      where (result, newRand) = runRand (genTiles n) rand

shuffleTileGen :: RandomGen g => g -> TileGenerator
shuffleTileGen firstRand = let
  shuffle :: RandomGen g => [a] -> Rand g [a]
  shuffle [] = return []
  shuffle (x:xs) = do
    r <- getRandomR (0, length xs)
    shuffled <- shuffle xs
    let (lead, trail) = splitAt r shuffled
    return $ lead ++ (x:trail)

  genHelper :: RandomGen g => g -> Int -> [TileColor] -> ([TileColor], TileGenerator)
  genHelper rand n tiles = let
    (result, newRand) = runRand (shuffle (take n tiles)) rand
    in
     (result, TileGen $ \nt -> genHelper newRand nt $ drop n tiles)
  in
   TileGen $ \numTiles -> genHelper firstRand numTiles (cycle tilecolors)

type TileLogic a = L.GameWire (V2 Float) Tile

blank :: TileLogic a
blank = pure Blank

stationary :: TileMap -> TileColor -> TileLogic a
stationary m color = mkGen_ $ \pos -> do
  L.renderSprite (m Map.! color) tileSz tileDepth pos
  return . Right $ Stationary color

countFromOne :: Float -> L.GameWire Float Float
countFromOne end = (countToOne end) >>> (mkSF_ (1.0 -))

countToOne :: Float -> L.GameWire Float Float
countToOne end = mkPure_ fn
  where
    fn t
      | t > end = Left mempty
      | otherwise = Right $ t / end

timer :: Float -> L.GameWire a Float
timer duration = timeF >>> (countFromOne duration)

swapping :: TileMap -> TileColor -> Bool -> TileLogic a
swapping m color movingLeft =
  let smoothstep :: L.GameWire Float Float
      smoothstep = mkSF_ $ \x -> let x3 = x*x*x in 6*x3*x*x - 15*x3*x + 10*x3

      movingWire :: L.GameWire (V2 Float, Float) Tile
      movingWire = mkGen_ $ \(pos@(V2 px py), t) -> do
        let otherPos =
              if movingLeft
                then (V2 (px - (fromIntegral blockSize)) py)
                else (V2 (px + (fromIntegral blockSize)) py)
        L.renderSprite (m Map.! color) tileSz tileDepth (lerp t otherPos pos)
        return . Right $ Moving color
  in
   (movingWire . (id &&& (smoothstep . timer gSwapTime))) --> (stationary m color)

falling :: TileMap -> TileColor -> TileLogic a
falling m color =
  let awareTimer :: Float -> L.GameWire a (Bool, Float)
      awareTimer duration =
        timeF >>> (lastFrame &&& (countToOne duration >>> modTime))
        where
          modTime = mkSF_ $ \t -> (t*t*(2-t))
          lastFrame = mkSF $ \dt t -> ((duration - t) < (dtime dt), lastFrame)

      movingWire :: L.GameWire (V2 Float, (Bool, Float)) Tile
      movingWire = mkGen_ $ \(pos@(V2 px py), (lastFrame, t)) -> do
        let renderPos = lerp t pos (V2 px $ py + (fromIntegral blockSize))
            (V2 _ yoff) = renderPos ^-^ pos
        L.renderSprite (m Map.! color) tileSz tileDepth renderPos
        return . Right $ Falling lastFrame yoff color
  in
   (movingWire . (id &&& (awareTimer gTileFallTime))) --> (stationary m color)

stillFalling :: TileMap -> TileColor -> Float -> TileLogic a
stillFalling m color offset =
  let reduce :: Float -> L.GameWire a (Bool, Float)
      reduce 0 = empty
      reduce dist = mkPure $ \t _ ->
        let travelling = min (gTileFallSpeed * (dtime t)) dist
            remaining = dist - travelling
        in (Right (remaining < travelling, remaining), reduce remaining)

      movingWire :: L.GameWire (V2 Float, (Bool, Float)) Tile
      movingWire = mkGen_ $ \(V2 px py, (lastFrame, yoff)) -> do
        L.renderSprite (m Map.! color) tileSz tileDepth (V2 px (py + yoff))
        return . Right $ Falling lastFrame yoff color
  in
   (movingWire . (id &&& (reduce $ fromIntegral blockSize + offset)))
   --> (stationary m color)

vanishing :: TileMap -> TileColor -> TileLogic a
vanishing m color =
  let alpha :: L.GameWire a Float
      alpha = timer gVanishTime

      render :: L.GameWire (V2 Float, Float) Tile
      render = mkGen_ $ \(pos, a) -> do
        L.renderSpriteWithAlpha (m Map.! color) a tileSz tileDepth pos
        return (Right Vanishing)
  in (render . (id &&& (when (>= 0) . alpha))) --> vanished --> blank

vanished :: TileLogic a
vanished = for gVanishedTime . pure Vanished

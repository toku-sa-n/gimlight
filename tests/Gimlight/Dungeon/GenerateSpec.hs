module Gimlight.Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                         (view)
import           Control.Monad.State                  (State, StateT, evalState,
                                                       evalStateT)
import           Data.Foldable                        (find)
import           Data.Map                             (empty)
import           Data.Maybe                           (isNothing)
import           Data.Tree                            (Tree (Node))
import           Gimlight.Coord                       (Coord)
import           Gimlight.Dungeon                     (Dungeon)
import qualified Gimlight.Dungeon                     as D
import           Gimlight.Dungeon.Generate            (floorTileIndex,
                                                       generateMultipleFloorsDungeon,
                                                       upStairsIndex)
import           Gimlight.Dungeon.Generate.Config     (Config, config,
                                                       getTileFilePath)
import           Gimlight.Dungeon.Identifier          (Identifier (Beaeve))
import           Gimlight.Dungeon.Map.Cell            (CellMap,
                                                       positionsAndActors,
                                                       tileIdLayer, topLayerAt)
import           Gimlight.Dungeon.Map.JSONReader      (readMapFile)
import           Gimlight.Dungeon.Map.Tile            (TileCollection)
import           Gimlight.Dungeon.Map.Tile.JSONReader (addTileFile)
import           Gimlight.IndexGenerator              (IndexGenerator,
                                                       generator)
import           Gimlight.Prelude
import           Gimlight.SetUp.TileFile              (tileFileForGeneration)
import           Linear.V2                            (V2 (V2))
import           System.Random                        (StdGen, mkStdGen)
import           Test.Hspec                           (Spec, describe, it,
                                                       runIO, shouldBe,
                                                       shouldSatisfy)
import           Test.Hspec.QuickCheck                (prop)

spec :: Spec
spec = do
    tc <- runIO $ addTileFile "tests/tiles/generate.json" empty
    expected <- runIO $ readMapFile "tests/maps/generate/seed_0.json"
    let result = generateSingleMap 0 tc
    describe "generateMultipleFloorsDungeon" $ do
        it "fills the lowest layer with the floor tile." $
            lowerLayerTiles result `shouldSatisfy`
            all (== Just (getTileFilePath cfg, floorTileIndex))
        it "generates the upper layer." $ upperLayerTiles result `shouldBe`
            upperLayerTiles expected
        testNoActorExistsOnUpStairs tc
        testUpstairsIsOnCorrectPosition tc
  where
    upperLayerTiles = tilesOf 0
    lowerLayerTiles = tilesOf 1
    tilesOf layerLevel = fmap ((!! layerLevel) . view tileIdLayer)

testUpstairsIsOnCorrectPosition :: TileCollection -> Spec
testUpstairsIsOnCorrectPosition tc =
    prop "upstairs appears on the correct position." $ \g ->
        let (d, c) = generateDungeonAndUpStairsPosition g tc
         in d ^? D.cellMap . topLayerAt c `shouldBe`
            Just (Just (tileFileForGeneration, upStairsIndex))

testNoActorExistsOnUpStairs :: TileCollection -> Spec
testNoActorExistsOnUpStairs tc =
    prop "does not locate an actor at the upstairs." $ \g ->
        let (d, c) = generateDungeonAndUpStairsPosition g tc
            l = positionsAndActors $ d ^. D.cellMap
         in isNothing $ find ((== c) . fst) l

generateSingleMap :: Int -> TileCollection -> CellMap
generateSingleMap g =
    view D.cellMap . fst . generateDungeonAndUpStairsPosition g

generateDungeonAndUpStairsPosition :: Int -> TileCollection -> (Dungeon, Coord)
generateDungeonAndUpStairsPosition g tc = (d, c)
  where
    (Node d _, c) = generateDungeonTreeAndUpstairsPosition g tc

generateDungeonTreeAndUpstairsPosition ::
       Int -> TileCollection -> (Tree Dungeon, Coord)
generateDungeonTreeAndUpstairsPosition g tc =
    extractDungeonTreeAndAscendingStairsPosition (mkStdGen g) $
    generateMultipleFloorsDungeon tc cfg Beaeve

extractDungeonTreeAndAscendingStairsPosition ::
       StdGen
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
    -> (Tree Dungeon, Coord)
extractDungeonTreeAndAscendingStairsPosition g =
    flip evalState g . flip evalStateT generator

cfg :: Config
cfg = config 1 10 (V2 3 3) (V2 10 10) tileFileForGeneration

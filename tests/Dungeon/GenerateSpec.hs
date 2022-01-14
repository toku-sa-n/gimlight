module Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                (_1, (^.))
import           Data.Array                  (array)
import           Data.Map                    (empty)
import           Data.Tree                   (Tree (Node))
import           Dungeon                     (dungeon)
import           Dungeon.Generate            (generateMultipleFloorsDungeon)
import           Dungeon.Generate.Config     (Config (Config, mapSize, maxRooms, numOfFloors, roomMaxSize, roomMinSize))
import           Dungeon.Identifier          (Identifier (Beaeve))
import           Dungeon.Map.Cell            (TileIdentifierLayer (TileIdentifierLayer),
                                              cellMap)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           IndexGenerator              (generator)
import           Linear.V2                   (V2 (V2))
import           System.Random               (mkStdGen)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = testGenerateSingleDungeon

testGenerateSingleDungeon :: Spec
testGenerateSingleDungeon = do
    tc <- runIO $ addTileFile "tiles/tiles.json" empty
    describe "generateMultipleFloorsDungeon" $
        it "generates a dungeon" $ result tc `shouldBe` expected
  where
    result tc =
        generateMultipleFloorsDungeon (mkStdGen 0) generator tc cfg Beaeve ^. _1
    expected = Node expectedDungeon []
    expectedDungeon =
        dungeon
            (cellMap
                 (array
                      (V2 0 0, V2 9 9)
                      [ (V2 x y, TileIdentifierLayer Nothing Nothing)
                      | x <- [0 .. 9]
                      , y <- [0 .. 9]
                      ]))
            Beaeve
    cfg =
        Config
            { numOfFloors = 1
            , maxRooms = 3
            , roomMinSize = 2
            , roomMaxSize = 3
            , mapSize = V2 10 10
            }

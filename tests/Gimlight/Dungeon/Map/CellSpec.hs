{-# LANGUAGE QuasiQuotes #-}

module Gimlight.Dungeon.Map.CellSpec
    ( spec
    ) where

import           Control.Lens              ((^.))
import           Data.String.QQ            (s)
import           Gimlight.Dungeon.Map.Cell (allWallTiles, tileIdentifierLayerAt,
                                            upper)
import           Gimlight.Dungeon.Map.Tile (wallTile)
import           Gimlight.SetUp.CellMap    (initCellMap)
import           Linear.V2                 (V2 (V2))
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.QuickCheck           (property)

spec :: Spec
spec = do
    testAllWallTiles
    testShowCellMap

testAllWallTiles :: Spec
testAllWallTiles =
    describe "allWallTiles" $
    it "returns a cell map filled with walls." $ property propertyFunc
  where
    propertyFunc (width, height) =
        checkFunc (width, height) (allWallTiles (V2 width height))
    checkFunc (width, height) cellMap =
        all
            (\(x, y) -> isWall (V2 x y) cellMap)
            [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    isWall c cellMap =
        maybe
            False
            ((== Just wallTile) . (^. upper))
            (tileIdentifierLayerAt c cellMap)

testShowCellMap :: Spec
testShowCellMap = do
    describe "CellMap" $
        it "show prints the detailed cell map information." $
        show initCellMap `shouldBe` expected
  where
    expected =
        [s|
Upper layer:
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+
|(0,1)|     |     |
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+

Lower layer:
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+
|     |     |     |
+-----+-----+-----+

Tile files:
0: dummy.json|]

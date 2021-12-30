module UI.Graphics.MapTilesSpec
    ( spec
    ) where

import           Codec.Picture           (convertRGBA8, readImage)
import           Data.Either.Combinators (fromRight')
import           Data.Map                (empty, insert)
import           SetUp.ImageFile         (singleTileImagePath,
                                          unitedTileImageFilePath)
import           SetUp.TileFile          (singleTileFile, unitedTileFile)
import           Test.Hspec              (Spec, describe, it, runIO, shouldBe)
import           UI.Graphics.MapTiles    (addTileFile)

spec :: Spec
spec = testAddTileFile

testAddTileFile :: Spec
testAddTileFile = do
    result <-
        runIO $
        addTileFile unitedTileFile unitedTileImageFilePath empty >>=
        addTileFile singleTileFile (singleTileImagePath 0)
    expected <-
        runIO $
        insertMultipleSeparatedFiles separatedFiles unitedTileFile empty >>=
        insertMultipleSeparatedFiles [singleTileImagePath 0] singleTileFile
    describe "addTileFile" $
        it
            "separate tile images in the tile file and adds all separated images." $
        -- `Image PixelRGBA8` does not implement `Show`. This is why we do not write `expected `shouldBe` True`.
        result == expected `shouldBe` True
  where
    insertMultipleSeparatedFiles fileNames insertAs m =
        foldl (flip (uncurry insert)) m . zip (zipWithIndex insertAs) <$>
        mapM readSingleSeparatedFile fileNames
    zipWithIndex name = zip (repeat name) [0 ..]
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    separatedFiles = fmap singleTileImagePath [0 :: Int .. 5]

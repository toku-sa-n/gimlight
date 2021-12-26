module UI.Graphics.MapTilesSpec
    ( spec
    ) where

import           Codec.Picture           (convertRGBA8, readImage)
import           Data.Either.Combinators (fromRight')
import           Data.Map                (empty, fromList)
import           Data.Maybe              (fromJust)
import           System.Directory        (canonicalizePath)
import           Test.Hspec              (Spec, describe, it, runIO, shouldBe)
import           UI.Graphics.MapTiles    (addTileFile)

spec :: Spec
spec = testAddTileFile

testAddTileFile :: Spec
testAddTileFile = do
    tileFile <- runIO $ canonicalizePath unitedImageFile
    result <- fmap fromJust . runIO $ addTileFile tileFile empty
    expected <- runIO $ readMultipleSeparatedFiles separatedFiles
    describe "addTileFile" $
        it
            "separate tile images in the tile file and adds all separated images." $
        -- `Image PixelRGBA8` does not implement `Show`. This is why we do not write `expected `shouldBe` True`.
        result == expected `shouldBe` True
  where
    separatedFiles =
        fmap
            (\x -> "tests/images/tiles/separated_" ++ show x ++ ".png")
            [0 :: Int .. 5]
    readMultipleSeparatedFiles fileNames =
        fromList . zip fileNameAndIndex <$>
        mapM readSingleSeparatedFile fileNames
    fileNameAndIndex = zip (repeat unitedImageFile) [0 ..]
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    unitedImageFile = "tests/images/tiles/united.png"

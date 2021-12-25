module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Data.Maybe             (fromJust)
import           Dungeon.Map.JSONReader (readMapFile)
import           System.Directory       (makeAbsolute)
import           System.FilePath        (equalFilePath)
import           Test.Hspec             (Spec, describe, it, runIO,
                                         shouldSatisfy)

spec :: Spec
spec = testReadMapFileReturnsTilePath

testReadMapFileReturnsTilePath :: Spec
testReadMapFileReturnsTilePath = do
    result <- runIO $ readMapFile "maps/beaeve.json"
    expected <- runIO $ makeAbsolute "maps/tiles.json"
    describe "readMapFile" .
        it "returns the path to the corresponding tile file." $
        snd (fromJust result) `shouldSatisfy` (`equalFilePath` expected)

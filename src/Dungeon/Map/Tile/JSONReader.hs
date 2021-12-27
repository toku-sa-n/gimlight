{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.Tile.JSONReader
    ( addTileFile
    ) where

import           Control.Lens     (filtered, has, only, (^..), (^?))
import           Control.Monad    ((>=>))
import           Data.Aeson.Lens  (_Bool, _Integer, _String, key, values)
import           Data.Map         (insert)
import           Data.Text        (Text, unpack)
import           Dungeon.Map.Tile (Tile, TileCollection, tile)
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.FilePath  (dropFileName, (</>))

addTileFile ::
       FilePath -> TileCollection -> IO (Maybe (TileCollection, FilePath))
addTileFile path tc = do
    json <- readFile path
    let relativePathToImageFile =
            (\x -> dropFileName path </> unpack x) <$> getImagePath json
    canonicalizedPathToJson <-
        canonicalizePath path >>= makeRelativeToCurrentDirectory
    case relativePathToImageFile of
        Just x -> do
            canonicalizedPath <-
                (canonicalizePath >=> makeRelativeToCurrentDirectory) x
            let newTc =
                    foldl
                        (\acc (idx, t) ->
                             insert (canonicalizedPathToJson, idx) t acc)
                        tc $
                    indexAndTile json
            return $ Just (newTc, canonicalizedPath)
        Nothing -> return Nothing

getImagePath :: String -> Maybe Text
getImagePath json = json ^? key "image" . _String

indexAndTile :: String -> [(Int, Tile)]
indexAndTile json =
    zip (getIds json) $ zipWith tile (getWalkable json) (getTransparent json)

getIds :: String -> [Int]
getIds json =
    fromInteger <$> json ^.. (key "tiles" . values . key "id") . _Integer

getTransparent :: String -> [Bool]
getTransparent = getBoolProperty "transparent"

getWalkable :: String -> [Bool]
getWalkable = getBoolProperty "walkable"

getBoolProperty :: Text -> String -> [Bool]
getBoolProperty property json =
    json ^.. key "tiles" . values . key "properties" . values .
    filtered (has (key "name" . _String . only property)) .
    key "value" .
    _Bool

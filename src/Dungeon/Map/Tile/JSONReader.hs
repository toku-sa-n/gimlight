{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.Tile.JSONReader
    ( addTileFile
    ) where

import           Control.Lens     (filtered, has, only, (^..), (^?))
import           Control.Monad    ((>=>))
import           Data.Aeson.Lens  (_Bool, _Integer, _String, key, values)
import           Data.Map         (insert)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text, unpack)
import           Dungeon.Map.Tile (Tile, TileCollection, tile)
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.FilePath  (dropFileName, (</>))

addTileFile :: FilePath -> TileCollection -> IO (TileCollection, FilePath)
addTileFile path tc = do
    json <- readFile path
    canonicalizedPathToJson <- canonicalizeAsRelative path
    canonicalizedPathToImage <-
        canonicalizeAsRelative $ dropFileName path </>
        unpack (getImagePath json)
    let newTc =
            foldl
                (\acc (idx, t) -> insert (canonicalizedPathToJson, idx) t acc)
                tc $
            indexAndTile json
    return (newTc, canonicalizedPathToImage)
  where
    canonicalizeAsRelative = canonicalizePath >=> makeRelativeToCurrentDirectory

getImagePath :: String -> Text
getImagePath json =
    fromMaybe
        (error "A tile file must associate with an image file.")
        (json ^? key "image" . _String)

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

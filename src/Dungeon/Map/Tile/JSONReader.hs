{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.Tile.JSONReader
    ( readTileFile
    ) where

import           Control.Lens     (filtered, has, only, (^..), (^?))
import           Data.Aeson.Lens  (_Bool, _Integer, _String, key, values)
import           Data.Map         (empty, insert)
import           Data.Text        (Text, unpack)
import           Dungeon.Map.Tile (Tile, TileCollection, tile)
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.FilePath  (dropFileName, (</>))

-- We set the initial values to prevent an `undefined element` panic on
-- comparisons.
readTileFile :: FilePath -> IO (Maybe (TileCollection, FilePath))
readTileFile path = do
    json <- readFile path
    case parseTileFile json of
        Just (tc, relativePath) -> do
            canonicalized <-
                canonicalizePath relativePath >>= makeRelativeToCurrentDirectory
            return $ Just (tc, canonicalized)
        Nothing -> return Nothing
  where
    parseTileFile json = do
        let tc =
                foldl (\acc (idx, t) -> insert idx t acc) empty $
                indexAndTile json
        imagePath <- unpack <$> getImagePath json
        return (tc, dropFileName path </> imagePath)

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

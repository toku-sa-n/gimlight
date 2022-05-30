{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Dungeon.Map.Tile.JSONReader
    ( addTileFile
    ) where

import           Codec.Picture             (Image (imageHeight, imageWidth),
                                            PixelRGBA8, convertRGBA8)
import           Codec.Picture.Extra       (crop)
import           Control.Applicative       (ZipList (ZipList, getZipList))
import           Control.Lens              (filtered, has, only)
import           Control.Monad             (guard, unless)
import           Data.Aeson.Lens           (_Integer, _String, key, values)
import           Data.Either               (fromRight)
import           Data.Map                  (insert)
import           Data.Text                 (unpack)
import           Gimlight.Codec.Picture    (readImage)
import           Gimlight.Data.Maybe       (expectJust)
import           Gimlight.Dungeon.Map.Tile (Tile, TileCollection, TileType,
                                            tile)
import           Gimlight.Prelude
import           Gimlight.System.Path      (canonicalizeToUnixStyleRelativePath,
                                            dropFileName, (</>))
import           Gimlight.UI.Draw.Config   (tileHeight, tileWidth)
import           Text.Read                 (readMaybe)

addTileFile :: FilePath -> TileCollection -> IO TileCollection
addTileFile path tc = do
    canonicalizedPathToJson <- canonicalizeToUnixStyleRelativePath path
    fmap
        (foldl (\acc (idx, t) -> insert (canonicalizedPathToJson, idx) t acc) tc)
        (indexAndTile path)

getImagePath :: Text -> Text
getImagePath json =
    expectJust
        "A tile file must associate with an image file."
        (json ^? key "image" . _String)

indexAndTile :: FilePath -> IO [(Int, Tile)]
indexAndTile path = do
    json <- readFile path
    let imagePath = dropFileName path </> getImagePath json
    unless (allTilesHaveNecessaryProperties json) $ error $ path <>
        ": Some tiles miss necessary properties."
    fmap
        (zip (getIds json) . getZipList . (tile <$> tileTypes json <*>))
        (images imagePath)
  where
    images = fmap ZipList . readAndCutTileImageFile
    tileTypes = ZipList . getTileType

allTilesHaveNecessaryProperties :: Text -> Bool
allTilesHaveNecessaryProperties json =
    all ((== tileCount) . length . (json &)) [getTileType]
  where
    tileCount = getTileCount json

getTileCount :: Text -> Int
getTileCount json =
    fromInteger $ expectJust "No tilecount entry." $ json ^? key "tilecount" .
    _Integer

getIds :: Text -> [Int]
getIds json =
    fromInteger <$> json ^.. (key "tiles" . values . key "id") . _Integer

getTileType :: Text -> [TileType]
getTileType = fmap readOrFail . getTextProperty "type"
  where
    readOrFail t =
        expectJust ("Unknow type: " <> showt t) $ readMaybe $ unpack t

getTextProperty :: Text -> Text -> [Text]
getTextProperty property json =
    json ^.. key "tiles" . values . key "properties" . values .
    filtered (has (key "name" . _String . only property)) .
    key "value" .
    _String

readAndCutTileImageFile :: FilePath -> IO [Image PixelRGBA8]
readAndCutTileImageFile = fmap cutTileMap . readTileImageFile

readTileImageFile :: FilePath -> IO (Image PixelRGBA8)
readTileImageFile path = do
    tileFile <- convertRGBA8 . fromRight (error noSuchImage) <$> readImage path
    guard $ isValidTileMapFile tileFile
    return tileFile
  where
    noSuchImage = path <> " not found."

isValidTileMapFile :: Image PixelRGBA8 -> Bool
isValidTileMapFile img =
    imageWidth img `mod` tileWidth == 0 && imageHeight img `mod` tileHeight == 0

cutTileMap :: Image PixelRGBA8 -> [Image PixelRGBA8]
cutTileMap img =
    [ crop (col * tileWidth) (row * tileHeight) tileWidth tileHeight img
    | row <- [0 .. rowsOfTilesInImage - 1]
    , col <- [0 .. columnsOfTilesInImage - 1]
    ]
  where
    columnsOfTilesInImage = imageWidth img `div` tileWidth
    rowsOfTilesInImage = imageHeight img `div` tileHeight

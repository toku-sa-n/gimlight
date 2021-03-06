-- For the map file specification, see https://doc.mapeditor.org/en/stable/reference/json-map-format/
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Gimlight.Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Exception         (assert)
import           Data.Aeson                (Value)
import           Data.Aeson.Lens           (_Array, _Integer, _String, key,
                                            values)
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Bits                 (Bits (testBit))
import           Data.List                 (find, sortBy, transpose)
import           Data.Vector               (Vector, toList)
import           Gimlight.Data.Maybe       (expectJust)
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer, cellMap)
import           Gimlight.Prelude
import           Gimlight.System.Path      (canonicalizeToUnixStyleRelativePath,
                                            dropFileName, (</>))
import           Linear.V2                 (V2 (V2))

readMapFile :: FilePath -> IO CellMap
readMapFile path = do
    json <- readFile path
    parseFile path json <$> getTileIdOfAllLayer json path

parseFile :: FilePath -> Text -> [TileIdLayer] -> CellMap
parseFile path json tiles = assertMapSizeIsCorrect convertToCellMap
  where
    V2 width height = getMapSize path json
    assertMapSizeIsCorrect = assert (height * width == length tiles)
    convertToCellMap =
        cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
        zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] tiles

getMapSize :: FilePath -> Text -> V2 Int
getMapSize mapPath json =
    case (fetch "width", fetch "height") of
        (Just w, Just h) -> fromIntegral <$> V2 w h
        _                -> error $ noWidthOrHeight mapPath
  where
    fetch k = json ^? key k . _Integer

getTileIdOfAllLayer :: Text -> FilePath -> IO [TileIdLayer]
getTileIdOfAllLayer json pathToMap
    -- From https://doc.mapeditor.org/en/stable/reference/tmx-map-format/:
    --
    -- > The order in which these layers appear is the order in which the layers are rendered by Tiled.
    --
    -- That is why we reverse here because we store tiles of each cell from top
    -- to bottom.
 =
    fmap (transpose . reverse) $ traverse (mapM rawIdToIdentifier) $
    getDataOfAllLayer json
  where
    rawIdToIdentifier 0 = return Nothing
    rawIdToIdentifier ident
        | transformationFlagsAreSet ident =
            error $ messageUsingTransformedTiles pathToMap
        | otherwise =
            (fmap Just . (\(x, y) -> (, y) <$> canonicalizeSource x)) .
            second (ident -) $
            whichMapOfId ident
    whichMapOfId ident =
        expectJust
            ("Invalid tile GID: " <> showt ident)
            (find ((ident >=) . snd) $ getSourceAndFirstGid json)
    -- The image path writtein in a map JSON file is a relative path from
    -- the map file. We need to convert it to a relative path from the
    -- project root directory.
    canonicalizeSource path =
        canonicalizeToUnixStyleRelativePath (dropFileName pathToMap </> path)

getSourceAndFirstGid :: Text -> [(FilePath, Int)]
getSourceAndFirstGid json = sortByGidInDescendingOrder $ zip sources firstGids
  where
    sortByGidInDescendingOrder = sortBy (\(_, a) (_, b) -> compare b a)
    sources = json ^.. key "tilesets" . values . key "source" . _String
    firstGids =
        fmap fromIntegral $ json ^.. key "tilesets" . values . key "firstgid" .
        _Integer

getDataOfAllLayer :: Text -> [[Int]]
getDataOfAllLayer json =
    fmap toList $ expectJust errMsg $ mapToInt (json ^.. lens)
  where
    lens = key "layers" . values . key "data" . _Array
    errMsg = "The data sections in layers contain non-integer values."

transformationFlagsAreSet :: Int -> Bool
transformationFlagsAreSet = or . flip fmap [29, 30, 31] . testBit

mapToInt :: [Vector Value] -> Maybe [Vector Int]
mapToInt = mapM (mapM (fmap fromInteger . (^? _Integer)))

messageUsingTransformedTiles :: FilePath -> Text
messageUsingTransformedTiles =
    (<> " contains rotated tiles. This game does not support them.")

noWidthOrHeight :: FilePath -> Text
noWidthOrHeight mapPath =
    "The map file " <> mapPath <>
    " does not contain both `width` and `height` fields."

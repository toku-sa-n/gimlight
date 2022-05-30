-- For the map file specification, see https://doc.mapeditor.org/en/stable/reference/json-map-format/
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Gimlight.Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Monad             (unless)
import           Data.Aeson.Lens           (_Array, _Integer, _String, key,
                                            values)
import           Data.Array                (array)
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Bits                 (Bits (testBit))
import           Data.List                 (find, sortBy, transpose)
import           Data.Vector               (Vector, fromList, toList)
import           Gimlight.Data.Maybe       (expectJust)
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer, cellMap)
import           Gimlight.Dungeon.Map.Tile (TileId)
import           Gimlight.Prelude
import           Gimlight.System.Path      (canonicalizeToUnixStyleRelativePath,
                                            dropFileName, (</>))
import           Linear.V2                 (V2 (V2))

readMapFile :: FilePath -> IO CellMap
readMapFile path = do
    json <- readFile path
    getTiles json path >>= parseFile json
  where
    parseFile json tiles = do
        let V2 width height = expectJust noWidthOrHeight $ getMapSize json
        unless (height * width == length tiles) $ error $
            invalidWidthHeight width height (length tiles)
        return $ cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
            zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
            toList tiles
    noWidthOrHeight =
        "The map file " <> path <>
        " does not contain both `width` and `height` fields."
    invalidWidthHeight w h l =
        "The multiplication of width and height of the map " <> path <>
        " does not equal to the number of tiles. The size is " <>
        showt (V2 w h) <>
        " but the length is " <>
        showt l <>
        "."

getMapSize :: Text -> Maybe (V2 Int)
getMapSize json =
    case (fetch "width", fetch "height") of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing
  where
    fetch k = json ^? key k . _Integer

-- From https://doc.mapeditor.org/en/stable/reference/tmx-map-format/:
--
-- > The order in which these layers appear is the order in which the layers are rendered by Tiled.
--
-- That is why we reverse here because we store tiles of each cell from top
-- to bottom.
getTiles :: Text -> FilePath -> IO (Vector TileIdLayer)
getTiles json =
    fmap (fromList . transpose . reverse . fmap toList) .
    getTileIdOfAllLayer json

getTileIdOfAllLayer :: Text -> FilePath -> IO [Vector (Maybe TileId)]
getTileIdOfAllLayer json pathToMap =
    traverse (mapM rawIdToIdentifier) $ getDataOfAllLayer json
  where
    rawIdToIdentifier 0 = return Nothing
    rawIdToIdentifier ident
        | transformationFlagsAreSet ident =
            error $ pathToMap <>
            " contains rotated tiles. This game does not support them."
        | otherwise =
            (fmap Just . (\(x, y) -> (, y) <$> canonicalizeIdentifier x)) .
            second (ident -) $
            expectJust
                ("Invalid tile GID: " <> showt ident)
                (find ((ident >=) . snd) $ getSourceAndFirstGid json)
    canonicalizeIdentifier path =
        canonicalizeToUnixStyleRelativePath (dropFileName pathToMap </> path)
    transformationFlagsAreSet = or . flip fmap [29, 30, 31] . testBit

getSourceAndFirstGid :: Text -> [(FilePath, Int)]
getSourceAndFirstGid json =
    sortBy (\(_, a) (_, b) -> compare b a) $ zip sources firstGids
  where
    sources = json ^.. key "tilesets" . values . key "source" . _String
    firstGids =
        fmap fromIntegral $ json ^.. key "tilesets" . values . key "firstgid" .
        _Integer

getDataOfAllLayer :: Text -> [Vector Int]
getDataOfAllLayer json =
    expectJust errMsg $
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)
  where
    errMsg = "The data sections in layers contain non-integer values."

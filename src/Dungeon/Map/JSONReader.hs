{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Lens     (Ixed (ix), (^..), (^?))
import           Control.Monad    (guard)
import           Data.Aeson.Lens  (_Array, _Integer, _String, key, values)
import           Data.Array       (array)
import           Data.Text        (unpack)
import           Data.Vector      (Vector, toList)
import qualified Data.Vector      as V
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile (TileId)
import           Linear.V2        (V2 (V2))
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.FilePath  (dropFileName, (</>))

readMapFile :: FilePath -> IO (Maybe (CellMap, FilePath))
readMapFile path = do
    json <- readFile path
    case parseFile json of
        Just (tc, relativePath) -> do
            canonicalized <-
                canonicalizePath relativePath >>= makeRelativeToCurrentDirectory
            return $ Just (tc, canonicalized)
        Nothing -> return Nothing
  where
    parseFile json = do
        V2 height width <- getMapSize json
        tiles <- getTiles json
        guard $ height * width == length tiles
        tileFilePath <- getTileFilePath json
        Just
            ( cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
              zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
              toList tiles
            , dropFileName path </> tileFilePath)

getTileFilePath :: String -> Maybe FilePath
getTileFilePath json =
    fmap unpack $ json ^? key "tilesets" . values . key "source" . _String

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> Maybe (Vector TileIdLayer)
getTiles json = V.zipWith TileIdLayer <$> uppers <*> lowers
  where
    lowers = getTileIdOfNthLayer 0 json
    uppers = getTileIdOfNthLayer 1 json

getTileIdOfNthLayer :: Int -> String -> Maybe (Vector (Maybe TileId))
getTileIdOfNthLayer n json = fmap rawIdToMaybe <$> getDataOfNthLayer n json
  where
    rawIdToMaybe 0 = Nothing
    rawIdToMaybe x = Just $ x - 1

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json = getDataOfAllLayer json >>= (^? ix n)

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)

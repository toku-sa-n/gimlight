{-# LANGUAGE LambdaCase        #-}
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
import           Dungeon.Map.Cell (CellMap,
                                   TileIdentifierLayer (TileIdentifierLayer),
                                   cellMap)
import           Dungeon.Map.Tile (TileIdentifier)
import           Linear.V2        (V2 (V2))
import           System.Directory (canonicalizePath,
                                   makeRelativeToCurrentDirectory)
import           System.FilePath  (dropFileName, (</>))

readMapFile :: FilePath -> IO (Maybe (CellMap, FilePath))
readMapFile path = do
    json <- readFile path
    tileFilePath json >>= \case
        Just x ->
            case parseFile json x of
                Just cm' -> return $ Just (cm', x)
                Nothing  -> return Nothing
        Nothing -> return Nothing
  where
    tileFilePath json =
        case getTileFilePath json of
            Just x -> do
                p <-
                    canonicalizePath (dropFileName path </> x) >>=
                    makeRelativeToCurrentDirectory
                return $ Just p
            Nothing -> return Nothing
    parseFile json canonicalizedPath = do
        V2 height width <- getMapSize json
        tiles <- getTiles json canonicalizedPath
        guard $ height * width == length tiles
        Just
            (cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
             zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
             toList tiles)

getTileFilePath :: String -> Maybe FilePath
getTileFilePath json =
    fmap unpack $ json ^? key "tilesets" . values . key "source" . _String

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> FilePath -> Maybe (Vector TileIdentifierLayer)
getTiles json path = V.zipWith TileIdentifierLayer <$> uppers <*> lowers
  where
    lowers = getTileIdOfNthLayer 0 json path
    uppers = getTileIdOfNthLayer 1 json path

getTileIdOfNthLayer ::
       Int -> String -> FilePath -> Maybe (Vector (Maybe TileIdentifier))
getTileIdOfNthLayer n json path = fmap rawIdToMaybe <$> getDataOfNthLayer n json
  where
    rawIdToMaybe 0 = Nothing
    rawIdToMaybe x = Just (path, x - 1)

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json = getDataOfAllLayer json >>= (^? ix n)

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)

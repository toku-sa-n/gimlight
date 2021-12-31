{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Dungeon.Map.JSONReader
    ( readMapTileImage
    ) where

import           Control.Lens                (Ixed (ix), (^..), (^?))
import           Control.Monad               (MonadPlus (mzero), unless)
import           Control.Monad.Except        (ExceptT (ExceptT), runExceptT)
import           Data.Aeson.Lens             (_Array, _Integer, _String, key,
                                              values)
import           Data.Array                  (array)
import           Data.Bifunctor              (Bifunctor (second))
import           Data.Either.Combinators     (maybeToRight)
import           Data.Foldable               (foldlM)
import           Data.List                   (find, sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (unpack)
import           Data.Vector                 (Vector, toList)
import qualified Data.Vector                 as V
import           Dungeon.Map.Cell            (CellMap,
                                              TileIdentifierLayer (TileIdentifierLayer),
                                              cellMap)
import           Dungeon.Map.Tile            (TileCollection, TileIdentifier)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           Linear.V2                   (V2 (V2))
import           System.Directory            (canonicalizePath,
                                              makeRelativeToCurrentDirectory)
import           System.FilePath             (dropFileName, (</>))

readMapTileImage :: TileCollection -> FilePath -> IO (CellMap, TileCollection)
readMapTileImage tc path =
    result >>= \case
        Right x -> return x
        Left x  -> error x
  where
    result =
        runExceptT $ do
            (cm, tileJsonPath) <- readMapFile path
            tc' <-
                ExceptT . fmap return $
                foldlM (flip addTileFile) tc tileJsonPath
            return (cm, tc')

readMapFile :: FilePath -> ExceptT String IO (CellMap, [FilePath])
readMapFile path = do
    json <- ExceptT . fmap return $ readFile path
    tileFilePath <- ExceptT . fmap return $ getAndCanonicalizeTileFilePath json
    cm <- getTiles json path >>= ExceptT . return . parseFile json
    return (cm, tileFilePath)
  where
    getAndCanonicalizeTileFilePath json = do
        let paths = getTileFilePath json
        mapM
            (\x ->
                 canonicalizePath (dropFileName path </> x) >>=
                 makeRelativeToCurrentDirectory)
            paths
    parseFile json tiles = do
        V2 width height <- maybeToRight noWidthOrHeight $ getMapSize json
        unless (height * width == length tiles) $ Left invalidWidthHeight
        Right
            (cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
             zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
             toList tiles)
    noWidthOrHeight =
        "The map file " ++ path ++
        " does not contain both `width` and `height` fields."
    invalidWidthHeight =
        "The multiplication of width and height of the map " ++ path ++
        " does not equal to the number of tiles."

getTileFilePath :: String -> [FilePath]
getTileFilePath json =
    fmap unpack $ json ^.. key "tilesets" . values . key "source" . _String

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> FilePath -> ExceptT String IO (Vector TileIdentifierLayer)
getTiles json pathToMap =
    case (uppers, lowers) of
        (Right x, Right y) ->
            ExceptT $ do
                x' <- x
                Right . V.zipWith TileIdentifierLayer x' <$> y
        _ -> error ""
  where
    lowers =
        maybeToRight (missingLayer "lower") $
        getTileIdOfNthLayer 0 json pathToMap
    uppers =
        maybeToRight (missingLayer "upper") $
        getTileIdOfNthLayer 1 json pathToMap
    missingLayer which =
        "The map file does not contain the " ++ which ++ " layer."

getTileIdOfNthLayer ::
       Int -> String -> FilePath -> Maybe (IO (Vector (Maybe TileIdentifier)))
getTileIdOfNthLayer n json pathToMap =
    case getDataOfNthLayer n json of
        Just x -> do
            let newData = mapM rawIdToIdentifier x
            Just newData
        Nothing -> mzero
  where
    rawIdToIdentifier 0 = return Nothing
    rawIdToIdentifier ident =
        (fmap Just . (\(x, y) -> (, y) <$> canonicalizeIdentifier x)) .
        second (ident -) $
        fromMaybe
            (error ("Invalid tile GID: " ++ show ident))
            (find (\(_, firstGid) -> ident >= firstGid) sourceAndGid)
    canonicalizeIdentifier path =
        canonicalizePath (dropFileName pathToMap </> path) >>=
        makeRelativeToCurrentDirectory
    sourceAndGid = getSourceAndFirstGid json

getSourceAndFirstGid :: String -> [(FilePath, Int)]
getSourceAndFirstGid json =
    sortBy (\(_, a) (_, b) -> compare b a) $ zip sources firstGids
  where
    sources =
        fmap unpack $ json ^.. key "tilesets" . values . key "source" . _String
    firstGids =
        fmap fromIntegral $ json ^.. key "tilesets" . values . key "firstgid" .
        _Integer

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json = getDataOfAllLayer json >>= (^? ix n)

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)

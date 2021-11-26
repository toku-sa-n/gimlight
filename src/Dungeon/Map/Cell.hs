{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Map.Cell
    ( CellMap
    , cellMap
    , allWallTiles
    , changeTileAt
    , walkableMap
    , widthAndHeight
    , isWalkableAt
    , transparentMap
    , tileIdAt
    , cellAt
    ) where

import           Actor            (Actor)
import           Control.Lens     (makeLenses, (&), (.~), (^.))
import           Coord            (Coord)
import           Data.Array       (Array, bounds, (!), (//))
import           Data.Binary      (Binary)
import           Data.Maybe       (isJust)
import qualified Dungeon.Map      as M
import           Dungeon.Map.Bool (BoolMap)
import           Dungeon.Map.Tile (TileCollection, TileId, isTransparent,
                                   isWalkable, wallTile)
import           GHC.Generics     (Generic)
import           Linear.V2        (V2 (V2))

data Cell =
    Cell
        { _tileId :: TileId
        , actor   :: Maybe Actor
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

newtype CellMap =
    CellMap (Array (V2 Int) Cell)
    deriving (Show, Ord, Eq, Generic)

instance Binary CellMap

cellMap :: Array (V2 Int) TileId -> CellMap
cellMap = CellMap . fmap (`Cell` Nothing)

allWallTiles :: V2 Int -> CellMap
allWallTiles wh = CellMap $ M.generate wh (const (Cell wallTile Nothing))

widthAndHeight :: CellMap -> V2 Int
widthAndHeight (CellMap m) = snd (bounds m) + V2 1 1

changeTileAt :: Coord -> TileId -> CellMap -> Maybe CellMap
changeTileAt c i (CellMap m)
    | isJust $ tileIdAt c (CellMap m) = Just $ CellMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileId .~ i

walkableMap :: TileCollection -> CellMap -> BoolMap
walkableMap tc (CellMap m) = isWalkable . ((tc !) . (^. tileId)) <$> m

transparentMap :: TileCollection -> CellMap -> BoolMap
transparentMap tc (CellMap m) = isTransparent . ((tc !) . (^. tileId)) <$> m

tileIdAt :: Coord -> CellMap -> Maybe TileId
tileIdAt c t = (^. tileId) <$> cellAt c t

cellAt :: Coord -> CellMap -> Maybe Cell
cellAt c (CellMap m)
    | c >= lower && c <= upper = Just $ m ! c
    | otherwise = Nothing
  where
    (lower, upper) = bounds m

isWalkableAt :: Coord -> TileCollection -> CellMap -> Bool
isWalkableAt c tc t =
    case tileIdAt c t of
        Just x  -> isWalkable (tc ! x)
        Nothing -> False

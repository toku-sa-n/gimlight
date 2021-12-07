module SetUp
    ( initCellMap
    , initTileCollection
    , playerPosition
    , orcWithoutItemsPosition
    , orcWithFullItemsPosition
    ) where

import           Actor            (inventoryItems, player)
import           Actor.Inventory  (addItem)
import           Actor.Monsters   (orc)
import           Control.Lens     ((%~))
import           Coord            (Coord)
import           Data.Array       (array, (//))
import           Data.Bifunctor   (Bifunctor (first))
import           Data.Maybe       (fromJust)
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap,
                                   locateActorAt, locateItemAt)
import           Dungeon.Map.Tile (TileCollection, tile)
import           IndexGenerator   (generator)
import           Item             (herb)
import           Linear.V2        (V2 (V2))

initCellMap :: CellMap
initCellMap =
    fromJust $
    locateActorAt
        p
        playerPosition
        (cellMap $
         array
             (V2 0 0, V2 2 1)
             [(V2 x y, emptyTile) | x <- [0 .. 2], y <- [0 .. 1]] //
         [(V2 0 1, unwalkable)]) >>=
    locateItemAt herb playerPosition >>=
    locateItemAt herb orcWithFullItemsPosition >>=
    locateActorAt orcWithoutItems orcWithoutItemsPosition >>=
    locateActorAt orcWithFullItems orcWithFullItemsPosition
  where
    (p, g) = player generator
    (orcWithoutItems, g') = orc g
    (orcWithFullItems, _) =
        iterate (first (inventoryItems %~ (fromJust . addItem herb))) (orc g') !!
        5
    emptyTile = TileIdLayer Nothing Nothing
    unwalkable = TileIdLayer (Just 1) Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 1) [(0, tile True True), (1, tile False True)]

playerPosition :: Coord
playerPosition = V2 0 0

orcWithoutItemsPosition :: Coord
orcWithoutItemsPosition = V2 1 0

orcWithFullItemsPosition :: Coord
orcWithFullItemsPosition = V2 2 0

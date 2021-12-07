module SetUp
    ( initCellMap
    , initTileCollection
    , playerPosition
    ) where

import           Actor            (player)
import           Actor.Monsters   (orc)
import           Coord            (Coord)
import           Data.Array       (array)
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
        (cellMap $ array (V2 0 0, V2 2 0) [(V2 x 0, emptyTile) | x <- [0 .. 2]]) >>=
    locateItemAt herb playerPosition >>=
    locateActorAt orcWithoutItems orcWithoutItemsPosition
  where
    (p, g) = player generator
    (orcWithoutItems, _) = orc g
    emptyTile = TileIdLayer Nothing Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]

playerPosition :: Coord
playerPosition = V2 0 0

orcWithoutItemsPosition :: Coord
orcWithoutItemsPosition = V2 1 0

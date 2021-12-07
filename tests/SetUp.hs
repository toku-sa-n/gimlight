{-# LANGUAGE OverloadedStrings #-}

module SetUp
    ( initCellMap
    , initTileCollection
    , playerPosition
    , orcWithoutItemsPosition
    , orcWithFullItemsPosition
    ) where

import           Actor            (Actor, inventoryItems, monster, player)
import           Actor.Identifier (Identifier (Orc))
import           Actor.Inventory  (addItem)
import           Actor.Monsters   (orc)
import           Actor.Status     (status)
import           Actor.Status.Hp  (hp)
import           Control.Lens     ((%~))
import           Coord            (Coord)
import           Data.Array       (array, (//))
import           Data.Bifunctor   (Bifunctor (first))
import           Data.Maybe       (fromJust)
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap,
                                   locateActorAt, locateItemAt)
import           Dungeon.Map.Tile (TileCollection, tile)
import           IndexGenerator   (IndexGenerator, generator)
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
             (V2 0 0, V2 2 3)
             [(V2 x y, emptyTile) | x <- [0 .. 2], y <- [0 .. 3]] //
         [(V2 0 1, unwalkable)]) >>=
    locateItemAt herb playerPosition >>=
    locateItemAt herb orcWithFullItemsPosition >>=
    locateActorAt orcWithoutItems orcWithoutItemsPosition >>=
    locateActorAt orcWithFullItems orcWithFullItemsPosition >>=
    locateActorAt s strongestOrcPosition >>=
    locateActorAt i intermediateOrcPosition >>=
    locateActorAt w weakestOrcPosition
  where
    (p, g) = player generator
    (w, g') = weakest g
    (i, g'') = intermediate g'
    (s, g''') = strongest g''
    (orcWithoutItems, g'''') = orc g'''
    (orcWithFullItems, _) =
        iterate
            (first (inventoryItems %~ (fromJust . addItem herb)))
            (orc g'''') !!
        5
    emptyTile = TileIdLayer Nothing Nothing
    unwalkable = TileIdLayer (Just 1) Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 1) [(0, tile True True), (1, tile False True)]

strongest :: IndexGenerator -> (Actor, IndexGenerator)
strongest g = monster g Orc (status (hp 100) 100 100) ""

intermediate :: IndexGenerator -> (Actor, IndexGenerator)
intermediate g = monster g Orc (status (hp 100) 50 50) ""

weakest :: IndexGenerator -> (Actor, IndexGenerator)
weakest g = monster g Orc (status (hp 1) 0 0) ""

playerPosition :: Coord
playerPosition = V2 0 0

orcWithoutItemsPosition :: Coord
orcWithoutItemsPosition = V2 1 0

orcWithFullItemsPosition :: Coord
orcWithFullItemsPosition = V2 2 0

strongestOrcPosition :: Coord
strongestOrcPosition = V2 1 2

intermediateOrcPosition :: Coord
intermediateOrcPosition = V2 0 3

weakestOrcPosition :: Coord
weakestOrcPosition = V2 1 3

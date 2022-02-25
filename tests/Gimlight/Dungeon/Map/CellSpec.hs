{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Dungeon.Map.CellSpec
    ( emptyTile
    , emptyCellMap
    , locateItemsActors
    ) where

import           Control.Monad.State       (execStateT)
import           Data.Array                (listArray)
import           Data.Either.Combinators   (fromRight')
import           Data.OpenUnion            (Union, typesExhausted, (@>))
import           Gimlight.Actor            (Actor)
import           Gimlight.Coord            (Coord)
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer),
                                            cellMap, locateActorAt,
                                            locateItemAt)
import           Gimlight.Item.SomeItem    (SomeItem)
import           Gimlight.SetUp.CellMap    (mockTileCollection)
import           Linear                    (V2 (V2))

emptyTile :: TileIdLayer
emptyTile = TileIdLayer Nothing Nothing

emptyCellMap :: V2 Int -> CellMap
emptyCellMap widthHeight =
    cellMap $ listArray (V2 0 0, widthHeight - V2 1 1) $ repeat emptyTile

locateItemsActors :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap -> CellMap
locateItemsActors xs cm = foldl helper cm xs
  where
    helper ncm (pos, x) =
        fromRight' $
        flip execStateT ncm $
        (itemFunc pos @> actorFunc pos @> typesExhausted) x
    actorFunc = apply locateActorAt
    itemFunc = apply locateItemAt
    apply f pos x = f mockTileCollection x pos

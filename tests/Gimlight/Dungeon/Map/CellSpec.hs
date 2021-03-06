{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Dungeon.Map.CellSpec
    ( spec
    , emptyTile
    , emptyCellMap
    , locateItemsActors
    , locateItemsActorsST
    , mapActorAt
    ) where

import           Control.Monad.State           (MonadState (get, put), StateT,
                                                evalStateT, execStateT)
import           Data.Array                    (listArray)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (Union, liftUnion,
                                                typesExhausted, (@>))
import           Gimlight.Actor                (Actor)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, Error (OutOfRange),
                                                TileIdLayer, cellMap,
                                                locateActorAt, locateItemAt,
                                                removeActorAt)
import           Gimlight.Dungeon.Map.Tile     (TileCollection)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.Item.Defined         (herb)
import           Gimlight.Item.SomeItem        (SomeItem)
import           Gimlight.Prelude
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec = testLocateItemAt

testLocateItemAt :: Spec
testLocateItemAt =
    describe "locateItemAt" $
    it "returns OutOfRange error if it tries to locate an item outside of a map." $
    result `shouldBe` Left OutOfRange
  where
    result = evalStateT locating cm
    locating = locateItemAt mockTileCollection (V2 5 5) (liftUnion herb)
    cm = emptyCellMap (V2 1 1)

emptyTile :: TileIdLayer
emptyTile = [Nothing, Nothing]

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
    apply f = f mockTileCollection

locateItemsActorsST ::
       [(Coord, Union '[ Actor, SomeItem])] -> StateT CellMap (Either Error) ()
locateItemsActorsST xs = do
    cm <- get
    let cm' = locateItemsActors xs cm
    put cm'

mapActorAt ::
       TileCollection
    -> Coord
    -> (Actor -> Actor)
    -> StateT CellMap (Either Error) ()
mapActorAt tc p f = removeActorAt p >>= locateActorAt tc p . f

{-# LANGUAGE LambdaCase #-}

module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Control.Lens              ((%%~), (&))
import           Control.Monad.State       (execStateT)
import           Data.Either.Combinators   (mapLeft, maybeToRight)
import           Dungeon                   (Dungeon, cellMap)
import           Dungeon.Map.Cell          (locateActorAt, updateExploredMap,
                                            updatePlayerFov)
import           Dungeon.Map.Tile          (TileCollection)
import           Dungeon.Predefined.Beaeve (beaeve)
import           IndexGenerator            (IndexGenerator)
import           Linear.V2                 (V2 (V2))

initDungeon ::
       TileCollection
    -> IndexGenerator
    -> IO (Dungeon, TileCollection, IndexGenerator)
initDungeon tc ig = do
    (beaeve', tc', ig'') <- beaeve tc ig'
    let d = unwrapRight $ beaeve' & cellMap %%~ initBeaeve tc'
    return (d, tc', ig'')
  where
    unwrapRight =
        \case
            Right x -> x
            Left x  -> error x
    initBeaeve tc' cm' =
        mapLeft show (execStateT (locateActorAt tc' player' (V2 5 5)) cm') >>=
        maybeToRight "Failed to update the player FoV." . updatePlayerFov tc' >>=
        Right . updateExploredMap
    (player', ig') = player ig

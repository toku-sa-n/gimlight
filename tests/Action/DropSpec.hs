{-# LANGUAGE OverloadedStrings #-}

module Action.DropSpec
    ( spec
    ) where

import           Action                     (ActionResult (ActionResult, killed, newDungeon, status),
                                             ActionStatus (Failed))
import           Action.Drop                (dropAction)
import           Actor                      (player)
import           Control.Monad.Trans.Writer (writer)
import           Data.Array                 (array)
import           Dungeon                    (dungeon)
import           Dungeon.Identifier         (Identifier (Beaeve))
import           Dungeon.Map.Cell           (TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile           (tile)
import           IndexGenerator             (generator)
import           Linear.V2                  (V2 (V2))
import           Localization               (multilingualText)
import           Test.Hspec                 (Spec, it, shouldBe)

spec :: Spec
spec =
    it "returns a Failed result if there is already an item at the player's foot" $
    result `shouldBe`
    writer
        ( ActionResult {status = Failed, newDungeon = d, killed = []}
        , [ multilingualText
                "There is already an item at your foot."
                "足元には既にアイテムがある。"
          ])
  where
    result = dropAction 1 (V2 0 0) a tc d
    (a, _) = player ig
    ig = generator
    tc = array (0, 0) [(0, tile True True)]
    d = dungeon cm Beaeve
    cm =
        cellMap $
        array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer (Just 0) (Just 0))]

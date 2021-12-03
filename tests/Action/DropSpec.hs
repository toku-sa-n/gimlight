{-# LANGUAGE OverloadedStrings #-}

module Action.DropSpec
    ( spec
    ) where

import           Action                     (ActionResult (ActionResult, killed, newDungeon, status),
                                             ActionStatus (Failed))
import           Action.Drop                (dropAction)
import           Actor                      (inventoryItems, player)
import           Actor.Inventory            (addItem)
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Writer (writer)
import           Data.Array                 (array)
import           Data.Maybe                 (fromJust)
import           Dungeon                    (dungeon, pushActor, pushItem)
import           Dungeon.Identifier         (Identifier (Beaeve))
import           Dungeon.Map.Cell           (TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile           (tile)
import           IndexGenerator             (generator)
import           Item                       (herb)
import           Linear.V2                  (V2 (V2))
import           Localization               (multilingualText)
import           Test.Hspec                 (Spec, it, shouldBe)

spec :: Spec
spec =
    it "returns a Failed result if there is already an item at the player's foot" $
    result `shouldBe`
    writer
        ( ActionResult
              { status = Failed
              , newDungeon = pushActor (V2 0 0) actorWithItem d
              , killed = []
              }
        , [ multilingualText
                "There is already an item at your foot."
                "足元には既にアイテムがある。"
          ])
  where
    result = dropAction 0 (V2 0 0) actorWithItem tc d
    actorWithItem =
        (\x -> a & inventoryItems .~ x)
            (fromJust $ addItem herb (a ^. inventoryItems))
    (a, _) = player ig
    ig = generator
    tc = array (0, 0) [(0, tile True True)]
    d = pushItem (V2 0 0) herb $ dungeon cm Beaeve
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

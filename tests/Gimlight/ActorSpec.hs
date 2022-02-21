module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Lens                ((%%~), (&))
import           Control.Monad.State         (evalState)
import           Data.Maybe                  (fromJust)
import           Gimlight.Actor              (equip, getWeapon, inventoryItems,
                                              player)
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item               (getName, sword)
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, describe, it, shouldBe)

spec :: Spec
spec = testEquipWeapon

testEquipWeapon :: Spec
testEquipWeapon =
    describe "equip" $
    it "equips a sword." $
    fmap getName (after >>= getWeapon) `shouldBe` Just T.sword
  where
    after = equip 0 before
    before = fromJust $ base & inventoryItems %%~ addItem sword
    base = evalState player generator

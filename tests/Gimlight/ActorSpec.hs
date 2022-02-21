module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Lens                ((%%~), (&))
import           Control.Monad.State         (evalState)
import           Data.Maybe                  (fromJust)
import           Gimlight.Actor              (equip, getItems, getWeapon,
                                              inventoryItems, player)
import qualified Gimlight.Actor              as A
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item               (Effect (Weapon), getEffect,
                                              getName, hammer, sword)
import qualified Gimlight.Item.Weapon        as W
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, context, describe, it,
                                              shouldBe)

spec :: Spec
spec =
    describe "equip" $ do
        testEquipWeapon
        testChangeWeapon

testEquipWeapon :: Spec
testEquipWeapon =
    context "When the actor does not equip anything." $ do
        it "equips a sword." $
            fmap getName (getWeapon after) `shouldBe` Just T.sword
        it "increases the power." $
            A.getPower after `shouldBe` A.getPower before + swordPower
        it "removes the sword from the inventory" $ getItems after `shouldBe` []
  where
    swordPower =
        case getEffect sword of
            Weapon x -> W.getPower x
            _        -> error "Not a weapon."
    after = fromJust $ equip 0 before
    before = fromJust $ base & inventoryItems %%~ addItem sword
    base = evalState player generator

testChangeWeapon :: Spec
testChangeWeapon =
    context "When the actor already equips a weapon." $
    it "equips a new weapon." $
    fmap getName (getWeapon after) `shouldBe` Just T.hammer
  where
    after = fromJust $ equip 0 before
    before =
        fromJust $ do
            x <- base & inventoryItems %%~ addItem hammer
            x & inventoryItems %%~ addItem sword >>= equip 0
    base = evalState player generator

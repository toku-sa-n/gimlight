{-# LANGUAGE ScopedTypeVariables #-}

module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Lens                ((%%~), (&))
import           Control.Monad.State         (evalState)
import           Data.Maybe                  (fromJust)
import           Data.OpenUnion              (liftUnion)
import           Gimlight.Actor              (Actor, equip, getArmor, getItems,
                                              getWeapon, inventoryItems, player)
import qualified Gimlight.Actor              as A
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item               (getName)
import qualified Gimlight.Item.Armor         as Armor
import           Gimlight.Item.Defined       (hammer, sword, woodenArmor)
import qualified Gimlight.Item.Weapon        as W
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, context, describe, it,
                                              shouldBe)

spec :: Spec
spec =
    describe "equip" $ do
        testEquipWeapon
        testChangeWeapon
        testEquipArmor

testEquipWeapon :: Spec
testEquipWeapon =
    context "When the actor does not equip anything." $ do
        it "equips a sword." $
            fmap getName (getWeapon after) `shouldBe` Just T.sword
        it "increases the power." $
            A.getPower after `shouldBe` A.getPower before + W.getPower sword
        it "removes the sword from the inventory" $ getItems after `shouldBe` []
  where
    after = fromJust $ equip 0 before
    before = fromJust $ base & inventoryItems %%~ addItem (liftUnion sword)

testChangeWeapon :: Spec
testChangeWeapon =
    context "When the actor already equips a weapon." $ do
        it "equips a new weapon." $
            fmap getName (getWeapon after) `shouldBe` Just T.hammer
        it "changes the power." $
            A.getPower after `shouldBe` A.getPower before - W.getPower sword +
            W.getPower hammer
        it "backs previously equpped weapon to the inventory." $
            getItems after `shouldBe` [liftUnion sword]
  where
    after = fromJust $ equip 0 before
    before =
        fromJust $ do
            x <- base & inventoryItems %%~ addItem (liftUnion hammer)
            x & inventoryItems %%~ addItem (liftUnion sword) >>= equip 0

testEquipArmor :: Spec
testEquipArmor =
    context "When the actor does not equip an armor." $ do
        it "equips an armor." $
            fmap getName (getArmor after) `shouldBe` Just T.woodenArmor
        it "increases the defence." $
            A.getDefence after `shouldBe` A.getDefence before +
            Armor.getDefence woodenArmor
  where
    after = fromJust $ equip 0 before
    before =
        fromJust $ base & inventoryItems %%~ addItem (liftUnion woodenArmor)

base :: Actor
base = evalState player generator

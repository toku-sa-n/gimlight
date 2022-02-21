module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Lens                ((%%~), (&))
import           Control.Monad.State         (evalState)
import           Data.Maybe                  (fromJust)
import           Gimlight.Actor              (equip, getWeapon, inventoryItems,
                                              player)
import qualified Gimlight.Actor              as A
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item               (Effect (Weapon), getEffect,
                                              getName, sword)
import qualified Gimlight.Item.Weapon        as W
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, describe, it, shouldBe)

spec :: Spec
spec = testEquipWeapon

testEquipWeapon :: Spec
testEquipWeapon =
    describe "equip" $ do
        it "equips a sword." $
            fmap getName (getWeapon after) `shouldBe` Just T.sword
        it "increases the power." $
            A.getPower after `shouldBe` A.getPower before + swordPower
  where
    swordPower =
        case getEffect sword of
            Weapon x -> W.getPower x
            _        -> error "Not a weapon."
    after = fromJust $ equip 0 before
    before = fromJust $ base & inventoryItems %%~ addItem sword
    base = evalState player generator

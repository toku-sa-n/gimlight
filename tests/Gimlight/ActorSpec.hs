module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Monad.State         (evalState)
import           Gimlight.Actor              (equip, getWeaponName, player)
import           Gimlight.IndexGenerator     (generator)
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, describe, it, shouldBe)

spec :: Spec
spec = testEquipWeapon

testEquipWeapon :: Spec
testEquipWeapon =
    describe "equip" $
    it "equips a sword." $ getWeaponName after `shouldBe` T.sword
  where
    after = equip 0 before
    before = evalState player generator

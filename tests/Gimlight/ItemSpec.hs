{-# LANGUAGE OverloadedStrings #-}

module Gimlight.ItemSpec
    ( goldenArmor
    , hammer
    ) where

import           Gimlight.Item               (Item, item)
import           Gimlight.Item.Armor         (Armor, armor)
import           Gimlight.Item.Weapon        (Weapon, weapon)
import qualified Gimlight.Localization.Texts as T

goldenArmor :: Item Armor
goldenArmor = item T.goldenArmor "images/items/golden_armor.png" (armor 8) False

hammer :: Item Weapon
hammer = item T.hammer "images/items/hammer.png" (weapon 8) False

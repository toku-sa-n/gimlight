{-# LANGUAGE OverloadedStrings #-}

module Gimlight.ItemSpec
    ( goldenArmor
    , hammer
    , goldenArmorName
    , hammerName
    ) where

import           Gimlight.Item         (Item, item)
import           Gimlight.Item.Armor   (Armor, armor)
import           Gimlight.Item.Weapon  (Weapon, weapon)
import           Gimlight.Localization (MultilingualText, multilingualText)
import           Gimlight.Prelude

goldenArmor :: Item Armor
goldenArmor =
    item goldenArmorName "images/items/golden_armor.png" (armor 8) False

hammer :: Item Weapon
hammer = item hammerName "images/items/hammer.png" (weapon 8) False

goldenArmorName :: MultilingualText
goldenArmorName = multilingualText "Golden armor" "金の鎧"

hammerName :: MultilingualText
hammerName = multilingualText "Hammer" "ハンマー"

{-# LANGUAGE OverloadedStrings #-}

module Gimlight.ItemSpec
    ( goldenArmor
    ) where

import           Gimlight.Item               (Item, item)
import           Gimlight.Item.Armor         (Armor, armor)
import qualified Gimlight.Localization.Texts as T

goldenArmor :: Item Armor
goldenArmor = item T.goldenArmor "images/items/golden_armor.png" (armor 8) False

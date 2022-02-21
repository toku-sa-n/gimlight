{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Item.Defined
    ( herb
    , sampleBook
    , sword
    , hammer
    , woodenArmor
    ) where

import           Gimlight.Item               (Item, item)
import           Gimlight.Item.Armor         (Armor, armor)
import           Gimlight.Item.Book          (Book)
import           Gimlight.Item.Heal          (Heal, heal)
import           Gimlight.Item.Weapon        (Weapon, weapon)
import qualified Gimlight.Localization.Texts as T

herb :: Item Heal
herb = item T.herb "images/herb.png" (heal 4) False

sampleBook :: Item Book
sampleBook = item T.sampleBook "images/book.png" T.sampleBookContent True

sword :: Item Weapon
sword = item T.sword "images/sword.png" (weapon 4) True

hammer :: Item Weapon
hammer = item T.hammer "images/hammer.png" (weapon 8) True

woodenArmor :: Item Armor
woodenArmor = item T.woodenArmor "images/wood.png" (armor 4) True

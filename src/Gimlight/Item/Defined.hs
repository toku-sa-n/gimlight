{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Item.Defined
    ( herb
    , sampleBook
    , sword
    , hammer
    , woodenArmor
    ) where

import           Gimlight.Item               (Effect (Armor, Book, Heal, Weapon),
                                              Item, item)
import           Gimlight.Item.Armor         (armor)
import           Gimlight.Item.Heal          (healHandler)
import           Gimlight.Item.Weapon        (weapon)
import qualified Gimlight.Localization.Texts as T

herb :: Item
herb = item T.herb "images/herb.png" (Heal $ healHandler 4) False

sampleBook :: Item
sampleBook = item T.sampleBook "images/book.png" (Book T.sampleBookContent) True

sword :: Item
sword = item T.sword "images/sword.png" (Weapon $ weapon 4) True

hammer :: Item
hammer = item T.hammer "images/hammer.png" (Weapon $ weapon 8) True

woodenArmor :: Item
woodenArmor = item T.woodenArmor "images/wood.png" (Armor $ armor 4) True

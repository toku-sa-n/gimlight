module Gimlight.Item.Armor
    ( Armor
    , armor
    , getArmor
    ) where

import           Gimlight.Item (Item, getEffect)

newtype Armor =
    Armor
        { defence :: Int
        }
    deriving (Show, Ord, Eq)

armor :: Int -> Armor
armor = Armor

getArmor :: Item Armor -> Int
getArmor = defence . getEffect

module Gimlight.Item.Armor
    ( Armor
    , armor
    , getDefence
    ) where

import           Gimlight.Item    (Item, getEffect)
import           Gimlight.Prelude

newtype Armor =
    Armor
        { defence :: Int
        }
    deriving (Show, Ord, Eq)

armor :: Int -> Armor
armor = Armor

getDefence :: Item Armor -> Int
getDefence = defence . getEffect

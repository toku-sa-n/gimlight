module Gimlight.Item.Armor
    ( Armor
    , armor
    , getArmor
    ) where

newtype Armor =
    Armor
        { defence :: Int
        }
    deriving (Show, Ord, Eq)

armor :: Int -> Armor
armor = Armor

getArmor :: Armor -> Int
getArmor = defence

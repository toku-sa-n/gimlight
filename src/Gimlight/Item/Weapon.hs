module Gimlight.Item.Weapon
    ( Weapon
    , weapon
    , getPower
    ) where

newtype Weapon =
    Weapon
        { power :: Int
        }
    deriving (Show, Ord, Eq)

weapon :: Int -> Weapon
weapon = Weapon

getPower :: Weapon -> Int
getPower = power

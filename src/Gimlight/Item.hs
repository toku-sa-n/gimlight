{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Item
    ( Item
    , item
    , Effect(..)
    , getName
    , getIconImagePath
    , getEffect
    , isUsableManyTimes
    ) where

import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Gimlight.Item.Armor   (Armor)
import           Gimlight.Item.Book    (Book)
import           Gimlight.Item.Heal    (HealHandler)
import           Gimlight.Item.Weapon  (Weapon)
import           Gimlight.Localization (MultilingualText)

data Effect
    = Heal HealHandler
    | Book Book
    | Weapon Weapon
    | Armor Armor
    deriving (Show, Ord, Eq, Generic)

data Item =
    Item
        { name            :: MultilingualText
        , iconImagePath   :: Text
        , effect          :: Effect
        , usableManyTimes :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

item :: MultilingualText -> Text -> Effect -> Bool -> Item
item n ip e u =
    Item {name = n, iconImagePath = ip, effect = e, usableManyTimes = u}

getName :: Item -> MultilingualText
getName Item {name = n} = n

getIconImagePath :: Item -> Text
getIconImagePath Item {iconImagePath = ip} = ip

getEffect :: Item -> Effect
getEffect Item {effect = e} = e

isUsableManyTimes :: Item -> Bool
isUsableManyTimes Item {usableManyTimes = u} = u

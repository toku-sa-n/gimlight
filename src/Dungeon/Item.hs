{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Item
    ( Item
    , herb
    ) where

import           Coord     (Coord)
import           Data.Text (Text)

data Item = Item
          { _position      :: Coord
          , _iconImagePath :: Text
          }

item :: Coord -> Text -> Item
item p ip = Item { _position = p
                 , _iconImagePath = ip
                 }

herb :: Coord -> Item
herb p = item p "images/herb.png"

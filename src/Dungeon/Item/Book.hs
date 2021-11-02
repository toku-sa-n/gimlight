{-# LANGUAGE DeriveGeneric #-}
module Dungeon.Item.Book
    ( BookHandler
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)
import           Localization (MultilingualText)

newtype BookHandler = BookHandler
                    { content :: MultilingualText
                    } deriving (Show, Ord, Eq, Generic)

instance Binary BookHandler

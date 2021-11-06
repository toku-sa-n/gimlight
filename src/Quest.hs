{-# LANGUAGE DeriveGeneric #-}

module Quest
    (
    ) where

import           GHC.Generics (Generic)

data QuestCollection =
    QuestCollection
        {
        }
    deriving (Show, Ord, Eq, Generic)

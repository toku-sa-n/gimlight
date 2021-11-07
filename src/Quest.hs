{-# LANGUAGE DeriveGeneric #-}

module Quest
    (
    ) where

import           GHC.Generics   (Generic)
import           Quest.KillBats (KillBats)

newtype QuestCollection =
    QuestCollection
        { killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

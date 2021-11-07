{-# LANGUAGE DeriveGeneric #-}

module Quest
    ( QuestCollection
    , questCollection
    ) where

import           GHC.Generics   (Generic)
import           Quest.KillBats (KillBats)
import qualified Quest.KillBats as KillBats

newtype QuestCollection =
    QuestCollection
        { killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

questCollection :: QuestCollection
questCollection = QuestCollection {killBats = KillBats.killBats}

{-# LANGUAGE DeriveGeneric #-}

module Quest
    ( QuestCollection
    , questCollection
    ) where

import           Data.Binary    (Binary)
import           GHC.Generics   (Generic)
import           Quest.KillBats (KillBats)
import qualified Quest.KillBats as KillBats

newtype QuestCollection =
    QuestCollection
        { killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary QuestCollection

questCollection :: QuestCollection
questCollection = QuestCollection {killBats = KillBats.killBats}

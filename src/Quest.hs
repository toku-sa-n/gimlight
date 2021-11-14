{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Quest
    ( QuestCollection
    , questCollection
    , handleWithTurnResult
    ) where

import           Actor.Identifier (Identifier)
import           Control.Lens     (makeLenses, (%~), (&))
import           Data.Binary      (Binary)
import           Dungeon          (Dungeon)
import           GHC.Generics     (Generic)
import           Quest.KillBats   (KillBats)
import qualified Quest.KillBats   as KillBats

newtype QuestCollection =
    QuestCollection
        { _killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''QuestCollection

instance Binary QuestCollection

questCollection :: QuestCollection
questCollection = QuestCollection {_killBats = KillBats.killBats}

handleWithTurnResult ::
       Dungeon -> [Identifier] -> QuestCollection -> QuestCollection
handleWithTurnResult currentDungeon killed qc =
    qc & killBats %~ KillBats.handleWithTurnResult currentDungeon killed

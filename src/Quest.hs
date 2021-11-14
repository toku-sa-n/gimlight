{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Quest
    ( QuestCollection
    , questCollection
    , handleWithTurnResult
    ) where

import qualified Actor.Identifier   as A
import           Control.Lens       (makeLenses, (%~), (&))
import           Data.Binary        (Binary)
import qualified Dungeon.Identifier as D
import           GHC.Generics       (Generic)
import           Quest.KillBats     (KillBats)
import qualified Quest.KillBats     as KillBats

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
       D.Identifier -> [A.Identifier] -> QuestCollection -> QuestCollection
handleWithTurnResult currentDungeon killed qc =
    qc & killBats %~ KillBats.handleWithTurnResult currentDungeon killed

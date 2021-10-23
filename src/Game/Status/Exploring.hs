module Game.Status.Exploring
    ( ExploringHandler
    ) where

import           Dungeon (Dungeon)
import           Log     (MessageLog)

data ExploringHandler = ExploringHandler
                      { currentDungeon :: Dungeon
                      , otherDungeons  :: [Dungeon]
                      , messageLog     :: MessageLog
                      }

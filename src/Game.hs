module Game
    ( Game(..)
    , isPlayerExploring
    ) where

import           Game.Config (Config)
import           Game.Status (GameStatus)
import qualified Game.Status as GS

data Game = Game
          { status :: GameStatus
          , config :: Config
          } deriving (Eq, Show)

isPlayerExploring :: Game -> Bool
isPlayerExploring Game { status = s } = GS.isPlayerExploring s

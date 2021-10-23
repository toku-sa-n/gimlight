module Game
    ( Game(..)
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
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

isPlayerTalking :: Game -> Bool
isPlayerTalking Game { status = s } = GS.isPlayerTalking s

isHandlingScene :: Game -> Bool
isHandlingScene Game { status = s } = GS.isHandlingScene s

isSelectingItemToUse :: Game -> Bool
isSelectingItemToUse Game { status = s } = GS.isSelectingItemToUse s

isTitle :: Game -> Bool
isTitle Game { status = s } = GS.isTitle s

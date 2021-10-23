module Game
    ( Game(..)
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , isGameOver
    , isSelectingLocale
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerEnteringTown
    , handlePlayerConsumingItem
    , saveStatus
    , loadStatus
    , finishTalking
    , destructTalking
    , destructHandlingScene
    ) where

import           Control.Monad.Trans.State (execState)
import           Game.Config               (Config)
import           Game.Status               (GameStatus)
import qualified Game.Status               as GS
import qualified Game.Status.Player        as GSP
import           Linear.V2                 (V2)
import qualified Save
import           Scene                     (Scene)
import           Talking                   (TalkWith)

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

isGameOver :: Game -> Bool
isGameOver Game { status = s } = GS.isGameOver s

isSelectingLocale :: Game -> Bool
isSelectingLocale Game { status = s } = GS.isSelectingLocale s

handlePlayerMoving :: V2 Int -> Game -> Game
handlePlayerMoving offset g@Game { status = s } =
    g { status = flip execState s $ GSP.handlePlayerMoving offset }

handlePlayerSelectingItemToUse :: Game -> Game
handlePlayerSelectingItemToUse g@Game { status = s } =
    g { status = GSP.handlePlayerSelectingItemToUse s }

handlePlayerEnteringTown :: Game -> Game
handlePlayerEnteringTown g@Game { status = s } =
    g { status = GS.enterTownAtPlayerPosition s }

handlePlayerPickingUp :: Game -> Game
handlePlayerPickingUp g@Game { status = s } =
    g { status = execState GSP.handlePlayerPickingUp s }

handlePlayerConsumingItem :: Game -> Game
handlePlayerConsumingItem g@Game { status = s } =
    g { status = execState GSP.handlePlayerConsumeItem s }

saveStatus :: Game -> IO ()
saveStatus Game { status = s } = Save.save s

loadStatus :: Game -> IO Game
loadStatus g = do
    s <-Save.load

    return g { status = s }

destructTalking :: Game -> (TalkWith, Game)
destructTalking g@Game{ status = s } = (tw, g { status = afterStatus })
    where (tw, afterStatus) = GS.destructTalking s

destructHandlingScene :: Game -> (Scene, Game)
destructHandlingScene g@Game { status = s } = (sc, g { status = afterScene })
    where (sc, afterScene) = GS.destructHandlingScene s

finishTalking :: Game -> Game
finishTalking g@Game { status = s } =
    g { status = GS.finishTalking s }

module Gimlight.UI.Types
    ( AppEvent(..)
    , GameWidgetEnv
    , GameWidgetNode
    , GameEventResponse
    ) where

import           Gimlight.GameModel (GameModel)
import           Gimlight.Prelude
import           Monomer            (AppEventResponse, WidgetEnv, WidgetNode)

data AppEvent
    = AppInit
    | AppKeyboardInput Text
    | NewGameLoaded GameModel
    | ShowNextScene
    | Tick
    deriving (Eq)

type GameWidgetEnv = WidgetEnv GameModel AppEvent

type GameWidgetNode = WidgetNode GameModel AppEvent

type GameEventResponse = AppEventResponse GameModel AppEvent

module Gimlight.Actor.Friendly
    ( friendly
    ) where

import           Control.Monad.State              (State)
import           Gimlight.Actor                   (Actor,
                                                   ActorKind (FriendlyNpc),
                                                   actor)
import           Gimlight.Actor.Identifier        (Identifier)
import           Gimlight.Actor.Status            (Status)
import           Gimlight.GameStatus.Talking.Part (TalkingPart)
import           Gimlight.IndexGenerator          (IndexGenerator)
import           Gimlight.Prelude

friendly ::
       Identifier
    -> Status
    -> TalkingPart
    -> Text
    -> Text
    -> State IndexGenerator Actor
friendly name st p = actor name st FriendlyNpc (Just p)

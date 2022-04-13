{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.Talking
    ( TalkingHandler
    , talkingHandler
    , getTalkingPartner
    , getTalkingPart
    , getExploringHandler
    , proceedTalking
    , selectPrevChoice
    , selectNextChoice
    ) where

import           Control.Lens                     ((&), (.~), (^.))
import           GHC.Generics                     (Generic)
import           Gimlight.Actor                   (Actor)
import           Gimlight.GameStatus.Exploring    (ExploringHandler, quests)
import           Gimlight.GameStatus.Talking.Part (TalkingPart,
                                                   proceedNonVisiblePartsIfNecessary)
import qualified Gimlight.GameStatus.Talking.Part as Part

data TalkingHandler =
    TalkingHandler
        { talkingPartner :: Actor
        , part           :: TalkingPart
        , afterTalking   :: ExploringHandler
        }
    deriving (Eq, Generic)

talkingHandler :: Actor -> TalkingPart -> ExploringHandler -> TalkingHandler
talkingHandler a p h = TalkingHandler a updatedPart updatedHandler
  where
    updatedHandler = h & quests .~ updatedQuests
    (updatedPart, updatedQuests) =
        case proceedNonVisiblePartsIfNecessary (h ^. quests) p of
            (Just x, h') -> (x, h')
            (Nothing, _) -> error "No part to show."

getTalkingPartner :: TalkingHandler -> Actor
getTalkingPartner (TalkingHandler a _ _) = a

getTalkingPart :: TalkingHandler -> TalkingPart
getTalkingPart (TalkingHandler _ p _) = p

getExploringHandler :: TalkingHandler -> ExploringHandler
getExploringHandler (TalkingHandler _ _ h) = h

proceedTalking :: TalkingHandler -> Either ExploringHandler TalkingHandler
proceedTalking (TalkingHandler a p at) =
    case Part.proceedTalking (at ^. quests) p of
        (Just next, updatedQuests) ->
            Right $ TalkingHandler a next (at & quests .~ updatedQuests)
        (Nothing, updatedQuests) -> Left $ at & quests .~ updatedQuests

selectPrevChoice :: TalkingHandler -> TalkingHandler
selectPrevChoice (TalkingHandler a p at) =
    TalkingHandler a (Part.selectPrevChoice p) at

selectNextChoice :: TalkingHandler -> TalkingHandler
selectNextChoice (TalkingHandler a p at) =
    TalkingHandler a (Part.selectNextChoice p) at

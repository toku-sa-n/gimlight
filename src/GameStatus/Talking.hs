{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking
    ( TalkingHandler
    , talkingHandler
    , getTalkingPartner
    , proceedTalking
    ) where

import           Actor                   (Actor)
import           Data.Binary             (Binary)
import           GHC.Generics            (Generic)
import           GameStatus.Exploring    (ExploringHandler)
import           GameStatus.Talking.Part (TalkingPart)
import qualified GameStatus.Talking.Part as Part

data TalkingHandler =
    TalkingHandler
        { talkingPartner :: Actor
        , part           :: TalkingPart
        , afterTalking   :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingHandler

talkingHandler :: Actor -> TalkingPart -> ExploringHandler -> TalkingHandler
talkingHandler = TalkingHandler

getTalkingPartner :: TalkingHandler -> Actor
getTalkingPartner (TalkingHandler a _ _) = a

proceedTalking :: TalkingHandler -> Either ExploringHandler TalkingHandler
proceedTalking (TalkingHandler a p at) =
    case Part.proceedTalking p of
        Just x  -> Right $ TalkingHandler a x at
        Nothing -> Left at

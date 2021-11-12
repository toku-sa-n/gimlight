{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking
    ( TalkingHandler
    , talkingHandler
    , TalkingElement(..)
    , getQuestion
    , getChoices
    , getSelectingIndex
    , getTalkingPartner
    , proceedTalking
    ) where

import           Actor                (Actor)
import           Data.Binary          (Binary)
import           GHC.Generics         (Generic)
import           GameStatus.Exploring (ExploringHandler)
import           Localization         (MultilingualText)

data SelectionHandler =
    SelectionHandler
        { question        :: MultilingualText
        , choicesAndNexts :: [(MultilingualText, Maybe TalkingElement)]
        , selectingIndex  :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectionHandler

newtype TalkingElement =
    Selection SelectionHandler
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingElement

data TalkingHandler =
    TalkingHandler
        { talkingPartner :: Actor
        , element        :: TalkingElement
        , afterTalking   :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingHandler

getQuestion :: SelectionHandler -> MultilingualText
getQuestion = question

getChoices :: SelectionHandler -> [MultilingualText]
getChoices = map fst . choicesAndNexts

getSelectingIndex :: SelectionHandler -> Int
getSelectingIndex = selectingIndex

talkingHandler :: Actor -> TalkingElement -> ExploringHandler -> TalkingHandler
talkingHandler = TalkingHandler

getTalkingPartner :: TalkingHandler -> Actor
getTalkingPartner (TalkingHandler a _ _) = a

proceedTalking :: TalkingHandler -> Either ExploringHandler TalkingHandler
proceedTalking (TalkingHandler a es at) =
    case es of
        Selection h ->
            case select h of
                Just x  -> Right $ TalkingHandler a x at
                Nothing -> Left at

select :: SelectionHandler -> Maybe TalkingElement
select (SelectionHandler _ xs n) = snd $ xs !! n

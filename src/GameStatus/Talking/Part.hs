{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking.Part
    ( TalkingPart
    , getQuestion
    , getChoices
    , getSelectingIndex
    , proceedTalking
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)
import           Localization (MultilingualText)

data SelectionHandler =
    SelectionHandler
        { question        :: MultilingualText
        , choicesAndNexts :: [(MultilingualText, Maybe TalkingPart)]
        , selectingIndex  :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectionHandler

newtype TalkingPart =
    Selection SelectionHandler
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingPart

getQuestion :: SelectionHandler -> MultilingualText
getQuestion = question

getChoices :: SelectionHandler -> [MultilingualText]
getChoices = map fst . choicesAndNexts

getSelectingIndex :: SelectionHandler -> Int
getSelectingIndex = selectingIndex

proceedTalking :: TalkingPart -> Maybe TalkingPart
proceedTalking (Selection h) = select h

select :: SelectionHandler -> Maybe TalkingPart
select (SelectionHandler _ xs n) = snd $ xs !! n

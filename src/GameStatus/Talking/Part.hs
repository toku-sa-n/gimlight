{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking.Part
    ( TalkingPart(..)
    , SelectionHandler
    , QuestInquiryHandler
    , selectionHandler
    , questInquiryHandler
    , getQuestion
    , getChoices
    , getSelectingIndex
    , proceedTalking
    , selectPrevChoice
    , selectNextChoice
    ) where

import           Data.Binary        (Binary)
import           Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe         (fromMaybe)
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)
import           Quest              (Inquiry, QuestCollection, Updater)
import qualified Quest

data UpdateQuestHandler =
    UpdateQuestHandler
        { updater :: Updater
        , after   :: Maybe TalkingPart
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary UpdateQuestHandler

data QuestInquiryHandler =
    QuestInquiryHandler
        { inquiry   :: Inquiry
        , trueThen  :: Maybe TalkingPart
        , falseThen :: Maybe TalkingPart
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary QuestInquiryHandler

data SelectionHandler =
    SelectionHandler
        { question        :: MultilingualText
        , choicesAndNexts :: NonEmpty (MultilingualText, Maybe TalkingPart)
        , selectingIndex  :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectionHandler

data TalkingPart
    = Selection SelectionHandler
    | QuestInquiry QuestInquiryHandler
    | UpdateQuest UpdateQuestHandler
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingPart

selectionHandler ::
       MultilingualText
    -> NonEmpty (MultilingualText, Maybe TalkingPart)
    -> SelectionHandler
selectionHandler q c = SelectionHandler q c 0

questInquiryHandler ::
       Inquiry -> Maybe TalkingPart -> Maybe TalkingPart -> QuestInquiryHandler
questInquiryHandler = QuestInquiryHandler

getQuestion :: SelectionHandler -> MultilingualText
getQuestion = question

getChoices :: SelectionHandler -> [MultilingualText]
getChoices = toList . NonEmpty.map fst . choicesAndNexts

getSelectingIndex :: SelectionHandler -> Maybe Int
getSelectingIndex (SelectionHandler _ cs n) =
    if null cs
        then Nothing
        else Just n

proceedTalking ::
       QuestCollection -> TalkingPart -> (Maybe TalkingPart, QuestCollection)
proceedTalking q (Selection h) = select q h
proceedTalking q p             = proceedNonVisibleParts q p

proceedNonVisibleParts ::
       QuestCollection -> TalkingPart -> (Maybe TalkingPart, QuestCollection)
proceedNonVisibleParts q (QuestInquiry (QuestInquiryHandler i t f))
    | Quest.inquiry i q =
        case t of
            Just x  -> proceedNonVisibleParts q x
            Nothing -> (Nothing, q)
    | otherwise =
        case f of
            Just x  -> proceedNonVisibleParts q x
            Nothing -> (Nothing, q)
proceedNonVisibleParts q (UpdateQuest (UpdateQuestHandler u af)) =
    case af of
        Just x  -> proceedNonVisibleParts updatedQuests x
        Nothing -> (Nothing, updatedQuests)
  where
    updatedQuests =
        fromMaybe
            (error "Failed to update the quest collection.")
            (Quest.update u q)
proceedNonVisibleParts _ _ = error "That part is visible."

selectPrevChoice :: TalkingPart -> TalkingPart
selectPrevChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx - 1) `mod` length cns
selectPrevChoice _ = error "We are not selecting anything."

selectNextChoice :: TalkingPart -> TalkingPart
selectNextChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx + 1) `mod` length cns
selectNextChoice _ = error "We are not selecting anything."

select ::
       QuestCollection
    -> SelectionHandler
    -> (Maybe TalkingPart, QuestCollection)
select q (SelectionHandler _ xs n) =
    case next of
        Just x  -> proceedNonVisibleParts q x
        Nothing -> (Nothing, q)
  where
    next = snd $ xs NonEmpty.!! n

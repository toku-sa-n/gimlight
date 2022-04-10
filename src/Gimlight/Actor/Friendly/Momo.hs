{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Actor.Friendly.Momo
    ( momo
    ) where

import           Control.Monad.State              (State)
import           Data.List.NonEmpty               (fromList)
import           Gimlight.Actor                   (Actor)
import           Gimlight.Actor.Friendly          (friendly)
import           Gimlight.Actor.Identifier        (Identifier (Momo))
import           Gimlight.Actor.Status            (status)
import           Gimlight.Actor.Status.Hp         (hp)
import           Gimlight.GameStatus.Talking.Part (TalkingPart (QuestInquiry, Selection, UpdateQuest),
                                                   questInquiryHandler,
                                                   selectionHandler,
                                                   updateQuestHandler)
import           Gimlight.IndexGenerator          (IndexGenerator)
import qualified Gimlight.Localization.Texts      as T
import           Gimlight.Quest                   (Inquiry (IsEnoughBatsKilled, IsKillBatsCompleted, IsKillBatsStarted),
                                                   Updater (CompleteKillBats, StartKillBats))

momo :: State IndexGenerator Actor
momo =
    friendly
        Momo
        st
        talking
        "images/electria.png"
        "images/upper_body/momo/default.png"
  where
    st = status (hp 1) 1 1

talking :: TalkingPart
talking = isQuestCompleted
  where
    isQuestCompleted =
        QuestInquiry $
        questInquiryHandler
            IsKillBatsCompleted
            (Just afterCompleted)
            (Just isQuestStarted)
    isQuestStarted =
        QuestInquiry $
        questInquiryHandler
            IsKillBatsStarted
            (Just isEnoughBatsKilled)
            (Just beforeStarted)
    afterCompleted =
        Selection $
        selectionHandler T.momoAfterCompletion $ fromList [(T.yes, Nothing)]
    isEnoughBatsKilled =
        QuestInquiry $
        questInquiryHandler
            IsEnoughBatsKilled
            (Just youDidIt)
            (Just questIsOnGoing)
    youDidIt =
        UpdateQuest $
        updateQuestHandler CompleteKillBats $
        Just $
        Selection $
        selectionHandler T.momoCompleted $ fromList [(T.yes, Nothing)]
    questIsOnGoing =
        Selection $
        selectionHandler T.momoNotCompleted $ fromList [(T.yes, Nothing)]
    beforeStarted =
        Selection $
        selectionHandler T.momoBeforeQuest $
        fromList [(T.yes, Just startQuest), (T.no, Just rejected)]
    startQuest =
        UpdateQuest $
        updateQuestHandler StartKillBats $
        Just $
        Selection $ selectionHandler T.momoAccept $ fromList [(T.yes, Nothing)]
    rejected =
        Selection $ selectionHandler T.momoReject $ fromList [(T.yes, Nothing)]

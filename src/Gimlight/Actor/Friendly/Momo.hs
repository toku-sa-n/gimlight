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
import           Gimlight.GameStatus.Talking.Part (TalkingPart,
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
        "images/walking_pictures/momo.png"
        "images/upper_body/momo/default.png"
  where
    st = status (hp 1) 1 1

talking :: TalkingPart
talking = isQuestCompleted
  where
    isQuestCompleted =
        questInquiryHandler
            IsKillBatsCompleted
            (Just afterCompleted)
            (Just isQuestStarted)
    isQuestStarted =
        questInquiryHandler
            IsKillBatsStarted
            (Just isEnoughBatsKilled)
            (Just beforeStarted)
    afterCompleted =
        selectionHandler T.momoAfterCompletion $ fromList [(T.yes, Nothing)]
    isEnoughBatsKilled =
        questInquiryHandler
            IsEnoughBatsKilled
            (Just youDidIt)
            (Just questIsOnGoing)
    youDidIt =
        updateQuestHandler CompleteKillBats $
        Just $ selectionHandler T.momoCompleted $ fromList [(T.yes, Nothing)]
    questIsOnGoing =
        selectionHandler T.momoNotCompleted $ fromList [(T.yes, Nothing)]
    beforeStarted =
        selectionHandler T.momoBeforeQuest $
        fromList [(T.yes, Just startQuest), (T.no, Just rejected)]
    startQuest =
        updateQuestHandler StartKillBats $
        Just $ selectionHandler T.momoAccept $ fromList [(T.yes, Nothing)]
    rejected = selectionHandler T.momoReject $ fromList [(T.yes, Nothing)]

{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions
    ( consumeAction
    , Action
    ) where

import           Control.Lens  ((^.))
import           Data.Text     (append, pack)
import           Dungeon       (Dungeon, pushActor)
import           Dungeon.Actor (Actor, healHp, name, removeNthItem)
import           Dungeon.Item  (healAmount)
import           Localization  (multilingualText)
import           Log           (MessageLog)

type Action = Actor -> Dungeon -> ((MessageLog, Bool), Dungeon)

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x ->
            (
                ([(e ^. name)
                    <> multilingualText
                        (" healed " `append` pack (show (x ^. healAmount)))
                        ("は" `append` pack (show (x ^. healAmount)) `append` "ポイント回復した．")
                ]
                , True)
                , pushActor (healHp newActor (x ^. healAmount)) d
            )
        Nothing -> (([multilingualText "What do you consume?" "何を使う？"], False), pushActor e d)

    where (item, newActor) = removeNthItem n e

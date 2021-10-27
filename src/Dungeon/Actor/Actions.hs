{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions
    ( pickUpAction
    , consumeAction
    , Action
    ) where

import           Control.Lens            ((&), (.~), (^.))
import           Data.Text               (append, pack)
import           Dungeon                 (Dungeon, popItemAt, pushActor,
                                          pushItem)
import           Dungeon.Actor           (Actor, healHp, inventoryItems, name,
                                          position, removeNthItem)
import           Dungeon.Actor.Inventory (addItem)
import           Dungeon.Item            (healAmount)
import qualified Dungeon.Item            as I
import           Localization            (multilingualText)
import           Log                     (MessageLog)

type Action = Actor -> Dungeon -> ((MessageLog, Bool), Dungeon)

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs ->
                    (
                        ([multilingualText "You got " "アイテムを入手しました：" <> (x ^. I.name)], True),
                        pushActor (e & inventoryItems .~ xs) dungeonAfterPickingUp
                    )
                Nothing ->
                    (([multilingualText "Your bag is full." "バッグは一杯だ．"], False), pushItem x $ pushActor e dungeonAfterPickingUp)
        Nothing -> (([multilingualText "You got nothing." "あなたは無を手に入れた．"], False), pushActor e dungeonAfterPickingUp)
    where (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d

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

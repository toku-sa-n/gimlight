{-# LANGUAGE OverloadedStrings #-}

module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Monad.Writer (tell)
import           Data.Maybe           (isJust)
import           Dungeon              (popItemAt, pushActor, pushItem)
import           Item                 (Item, getName)
import           Localization         (multilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e tiles d
    | itemExists = do
        tell
            [ multilingualText
                  "There is already an item at your foot."
                  "足元には既にアイテムがある。"
            ]
        return $ ActionResult Failed (pushActor position e d) []
    | otherwise =
        case item of
            Just x -> dropItem x position newActor tiles d
            Nothing -> do
                tell [T.whatToDrop]
                return $ ActionResult Failed (pushActor position e d) []
  where
    (item, newActor) = removeNthItem n e
    itemExists = (\(x, _) -> isJust x) $ popItemAt position d

dropItem :: Item -> Action
dropItem item position actor _ dungeon = do
    tell [T.youDropped $ getName item]
    return $
        ActionResult
            Ok
            (pushActor position actor $ pushItem position item dungeon)
            []

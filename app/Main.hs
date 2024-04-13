{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe                   #-}

module Main
  ( main
  ) where

import           Data.Kind (Type)
import           Monomer   (AppEventResponse, WidgetEnv, WidgetNode, label,
                            startApp)
import           Prelude   (Eq, IO)

type GameModel :: Type

data GameModel =
  GameModel
  deriving stock (Eq)

handleEvent ::
     WidgetEnv GameModel ()
  -> WidgetNode GameModel ()
  -> GameModel
  -> ()
  -> [AppEventResponse GameModel ()]
handleEvent _ _ _ _ = []

buildUI :: WidgetEnv GameModel () -> GameModel -> WidgetNode GameModel ()
buildUI _ _ = label "Press q to quit"

main :: IO ()
main = startApp initialModel handleEvent buildUI config
  where
    initialModel = GameModel
    config :: [a]
    config = []

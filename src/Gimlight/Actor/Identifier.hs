{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Actor.Identifier
    ( Identifier(..)
    , toName
    ) where

import           GHC.Generics                (Generic)
import           Gimlight.Localization       (MultilingualText)
import qualified Gimlight.Localization.Texts as T

data Identifier
    = Orc
    | Troll
    | Momo
    | Player
    deriving (Show, Ord, Eq, Generic)

toName :: Identifier -> MultilingualText
toName Orc    = T.orc
toName Troll  = T.troll
toName Momo   = T.momo
toName Player = T.player

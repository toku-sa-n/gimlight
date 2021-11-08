{-# LANGUAGE DeriveGeneric #-}

module Quest.KillBats
    ( KillBats
    , killBats
    , questCompleted
    , incrementCount
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

newtype KillBats =
    KillBats
        { getRemaining :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary KillBats

killBats :: KillBats
killBats = KillBats 0

questCompleted :: KillBats -> Bool
questCompleted k = getRemaining k >= quota

incrementCount :: KillBats -> KillBats
incrementCount (KillBats k) = KillBats (k + 1)

quota :: Int
quota = 3

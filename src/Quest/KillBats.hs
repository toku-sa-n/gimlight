{-# LANGUAGE DeriveGeneric #-}

module Quest.KillBats
    ( KillBats
    , killBats
    , handleWithTurnResult
    , questCompleted
    ) where

import qualified Actor.Identifier   as A
import           Data.Binary        (Binary)
import qualified Dungeon.Identifier as D
import           GHC.Generics       (Generic)

newtype KillBats =
    KillBats
        { getRemaining :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary KillBats

killBats :: KillBats
killBats = KillBats 0

handleWithTurnResult :: D.Identifier -> [A.Identifier] -> KillBats -> KillBats
handleWithTurnResult d killed k =
    if d == D.BatsCave
        then iterate incrementCount k !! length killed
        else k

questCompleted :: KillBats -> Bool
questCompleted k = getRemaining k >= quota

incrementCount :: KillBats -> KillBats
incrementCount (KillBats k) = KillBats (k + 1)

quota :: Int
quota = 3

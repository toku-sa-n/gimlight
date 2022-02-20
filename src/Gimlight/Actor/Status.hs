{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Actor.Status
    ( Status
    , status
    , getLevel
    , getHp
    , getMaxHp
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , attackFromTo
    , healHp
    , getPower
    , getDefence
    ) where

import           Data.Binary                      (Binary)
import           Data.Maybe                       (isNothing)
import           GHC.Generics                     (Generic)
import           Gimlight.Actor.Status.Experience (Experience, gainExperience)
import qualified Gimlight.Actor.Status.Experience as E
import           Gimlight.Actor.Status.Hp         (Hp)
import qualified Gimlight.Actor.Status.Hp         as HP
import           Gimlight.Localization            (MultilingualText)
import qualified Gimlight.Localization.Texts      as T
import           Gimlight.Log                     (MessageLog)
import qualified Gimlight.Log                     as M

data Status =
    Status
        { _hp         :: Hp
        , _power      :: Int
        , _defence    :: Int
        , _experience :: Experience
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Status

status :: Hp -> Int -> Int -> Status
status h p d = Status h p d E.experience

getHp :: Status -> Int
getHp Status {_hp = h} = HP.getHp h

getMaxHp :: Status -> Int
getMaxHp Status {_hp = h} = HP.getMaxHp h

attackFromTo ::
       Status
    -> Status
    -> ( Status
       , Maybe Status
       , MultilingualText -> MultilingualText -> MessageLog)
attackFromTo attacker defender = (newAttacker, newDefender, message)
  where
    damage = max 0 $ getPower attacker - getDefence defender
    newAttacker =
        attacker
            { _power = getPower attacker + levelUp
            , _defence = getDefence attacker + levelUp
            , _experience = newAttackerExp
            }
    newDefender = receiveDamage damage defender
    (levelUp, newAttackerExp)
        | isNothing newDefender =
            gainExperience experiencePointAmount (_experience attacker)
        | otherwise = (0, _experience attacker)
    experiencePointAmount = getPower defender + getDefence defender
    message =
        case newDefender of
            Just _  -> notKilledMessage
            Nothing -> killedMessage
    notKilledMessage a d
        | damage > 0 = [M.message $ T.damagedMessage damage a d]
        | otherwise = [M.message $ T.noDamageMessage a d]
    killedMessage a d =
        map M.message [T.damagedMessage damage a d, T.deathMessage d] ++
        [M.message $ T.levelUp a (getLevel newAttacker) | levelUp > 0]

healHp :: Int -> Status -> Status
healHp amount a@Status {_hp = h} = a {_hp = HP.healHp amount h}

receiveDamage :: Int -> Status -> Maybe Status
receiveDamage damage a@Status {_hp = h} =
    (\x -> a {_hp = x}) <$> HP.receiveDamage damage h

getPower :: Status -> Int
getPower = _power

getDefence :: Status -> Int
getDefence = _defence

getLevel :: Status -> Int
getLevel Status {_experience = e} = E.getLevel e

getCurrentExperiencePoint :: Status -> Int
getCurrentExperiencePoint Status {_experience = e} =
    E.getCurrentExperiencePoint e

getExperiencePointForNextLevel :: Status -> Int
getExperiencePointForNextLevel Status {_experience = e} = E.pointForNextLevel e

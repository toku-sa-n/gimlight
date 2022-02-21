{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gimlight.Actor
    ( Actor
    , player
    , getIndex
    , getIdentifier
    , getLevel
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , getHp
    , getMaxHp
    , getPower
    , getDefence
    , getTalkingPart
    , monster
    , isPlayer
    , isMonster
    , isFriendlyNpc
    , ActorKind(FriendlyNpc)
    , attackFromTo
    , actor
    , pathToDestination
    , standingImagePath
    , walkingImagePath
    , healHp
    , inventoryItems
    , getItems
    , target
    , equip
    , getWeapon
    ) where

import           Control.Lens                     (makeLenses, (%~), (&), (.~),
                                                   (?~), (^.))
import           Control.Monad.State              (State)
import           Control.Monad.Writer             (MonadWriter (writer), Writer)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Gimlight.Actor.Identifier        (Identifier, toName)
import qualified Gimlight.Actor.Identifier        as Identifier
import           Gimlight.Actor.Status            (Status)
import qualified Gimlight.Actor.Status            as S
import           Gimlight.Actor.Status.Hp         (hp)
import           Gimlight.Coord                   (Coord)
import           Gimlight.GameStatus.Talking.Part (TalkingPart)
import           Gimlight.IndexGenerator          (Index, IndexGenerator,
                                                   generate)
import           Gimlight.Inventory               (Inventory, addItem,
                                                   inventory, removeNthItem)
import qualified Gimlight.Inventory               as I
import           Gimlight.Item                    (Effect (Weapon), Item,
                                                   getEffect)
import qualified Gimlight.Item.Weapon             as W
import           Gimlight.Log                     (MessageLog)

data ActorKind
    = Player
    | FriendlyNpc
    | Monster
    deriving (Show, Ord, Eq, Generic)

data Actor =
    Actor
        { _index             :: Index
        , _identifier        :: Identifier
        , _status            :: Status
        , _pathToDestination :: [Coord]
        , _actorKind         :: ActorKind
        , _talk              :: Maybe TalkingPart
        , _walkingImagePath  :: Text
        , _standingImagePath :: Text
        , _inventoryItems    :: Inventory
        , _target            :: Maybe Index
        , _weapon            :: Maybe Item
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Actor

actor ::
       Identifier
    -> Status
    -> ActorKind
    -> Maybe TalkingPart
    -> Text
    -> Text
    -> State IndexGenerator Actor
actor id' st ak talkMessage' walkingImagePath' standingImagePath' = do
    idx <- generate
    return
        Actor
            { _index = idx
            , _identifier = id'
            , _status = st
            , _pathToDestination = []
            , _talk = talkMessage'
            , _walkingImagePath = walkingImagePath'
            , _standingImagePath = standingImagePath'
            , _actorKind = ak
            , _inventoryItems = inventory 5
            , _target = Nothing
            , _weapon = Nothing
            }

monster :: Identifier -> Status -> Text -> State IndexGenerator Actor
monster name' st walking =
    actor name' st Monster Nothing walking "images/sample_standing_picture.png"

player :: State IndexGenerator Actor
player =
    actor
        Identifier.Player
        st
        Player
        Nothing
        "images/player.png"
        "images/sample_standing_picture.png"
  where
    st = S.status (hp 30) 5 2

isPlayer :: Actor -> Bool
isPlayer e = e ^. actorKind == Player

isMonster :: Actor -> Bool
isMonster e = e ^. actorKind == Monster

isFriendlyNpc :: Actor -> Bool
isFriendlyNpc e = e ^. actorKind == FriendlyNpc

getIdentifier :: Actor -> Identifier
getIdentifier a = a ^. identifier

getHp :: Actor -> Int
getHp e = S.getHp $ e ^. status

getMaxHp :: Actor -> Int
getMaxHp e = S.getMaxHp $ e ^. status

getTalkingPart :: Actor -> Maybe TalkingPart
getTalkingPart a = a ^. talk

attackFromTo :: Actor -> Actor -> (Writer MessageLog) (Actor, Maybe Actor)
attackFromTo attacker defender = writer ((newAttacker, newDefender), msg)
  where
    (newAttackerStatus, newDefenderStatus, msgFunc) =
        S.attackFromTo (attacker ^. status) (defender ^. status)
    newAttacker = attacker & status .~ newAttackerStatus
    newDefender = (\x -> defender & status .~ x) <$> newDefenderStatus
    msg =
        msgFunc
            (toName $ getIdentifier attacker)
            (toName $ getIdentifier defender)

healHp :: Int -> Actor -> Actor
healHp amount a = a & status %~ S.healHp amount

getIndex :: Actor -> Index
getIndex a = a ^. index

getLevel :: Actor -> Int
getLevel a = S.getLevel $ a ^. status

getCurrentExperiencePoint :: Actor -> Int
getCurrentExperiencePoint a = S.getCurrentExperiencePoint $ a ^. status

getExperiencePointForNextLevel :: Actor -> Int
getExperiencePointForNextLevel a =
    S.getExperiencePointForNextLevel $ a ^. status

getPower :: Actor -> Int
getPower a = S.getPower (a ^. status) + weaponPower
  where
    weaponPower =
        case fmap getEffect (a ^. weapon) of
            Just (Weapon x) -> W.getPower x
            _               -> 0

getDefence :: Actor -> Int
getDefence a = S.getDefence $ a ^. status

getItems :: Actor -> [Item]
getItems a = I.getItems $ a ^. inventoryItems

getWeapon :: Actor -> Maybe Item
getWeapon a = a ^. weapon

equip :: Int -> Actor -> Maybe Actor
equip n a =
    case (w, newInventory) of
        (Just x, Just inv) -> Just $ a & weapon ?~ x & inventoryItems .~ inv
        _                  -> Nothing
  where
    newInventory =
        case currentWeapon of
            Just x  -> addItem x inventoryWithoutWeapon
            Nothing -> Just inventoryWithoutWeapon
    currentWeapon = a ^. weapon
    (w, inventoryWithoutWeapon) = removeNthItem n (a ^. inventoryItems)

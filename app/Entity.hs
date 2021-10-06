module Entity
    ( Entity
    , player
    , getHp
    , updateHp
    , monster
    ) where

import           Control.Lens  ((&), (&~), (.=), (.~), (^.))
import           Coord         (Coord)
import           Dungeon.Types (Entity, RenderOrder (ActorEntity, Corpse),
                                actor, blocksMovement, char, hp, isAlive, maxHp,
                                name, renderOrder)

monster :: Coord -> String -> String -> Int -> Int -> Int -> Entity
monster position char' name' maxHp' defence power =
        actor position char' name' maxHp' defence power True True False ActorEntity True ""

player :: Coord -> Entity
player c = actor c "@" "Player" 30 2 5 True True True ActorEntity False ""

getHp :: Entity -> Int
getHp e = e ^. hp

updateHp :: Entity -> Int -> Entity
updateHp e newHp =
        let newHpInRange = max 0 $ min (e ^. maxHp) newHp
        in if newHpInRange == 0 && e ^. isAlive
               then die e
               else e & hp .~ newHpInRange

die :: Entity -> Entity
die e = e &~ do
    char .= "%"
    blocksMovement .= False
    name .= "remains of " ++ (e ^. name)
    isAlive .= False
    renderOrder .= Corpse

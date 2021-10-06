module Entity.Friendly
    ( electria
    ) where

import           Coord         (Coord)
import           Dungeon.Types (RenderOrder (ActorEntity), actor)
import           Entity        (Entity)

electria :: Coord -> Entity
electria position = friendly position "1" "Electria" 50 50 50 "How's it going, Ruskell?"

friendly :: Coord -> String -> String -> Int -> Int -> Int -> String -> Entity
friendly position char name maxHp defence power = actor position char name maxHp defence power True True False ActorEntity False

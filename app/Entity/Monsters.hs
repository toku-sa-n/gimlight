module Entity.Monsters
    ( orc
    , troll
    ) where
import           Coord  (Coord)
import           Entity (Entity, monster)

orc :: Coord -> Entity
orc c = monster c "o" "Orc" 10 0 3

troll :: Coord -> Entity
troll c = monster c "T" "Troll" 16 1 4

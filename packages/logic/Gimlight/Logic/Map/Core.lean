module

public import Gimlight.Logic.Position.Basic
public import Gimlight.Logic.Dimensions

namespace Gimlight

public inductive Tile where
  | wall
  | floor
deriving DecidableEq, Repr

public structure FloorReachable (tiles : Array Tile) (width : Nat)
    (source target : Position) : Prop where
  fromFloor : tiles[source.y * width + source.x]? = some .floor
  toFloor : tiles[target.y * width + target.x]? = some .floor

public structure Map where private mk ::
  public dimensions : Dimensions
  -- Tiles are stored in row-major order at `y * dimensions.width + x`. A flat array lets
  -- `tiles.size = dimensions.width * dimensions.height` guarantee a rectangular map, without ragged
  -- rows or separate length proofs for every row as with `Array (Array Tile)`.
  private tiles : Array Tile
  private tilesSize : tiles.size = dimensions.width * dimensions.height
  private allFloorsConnected : ∀ source target,
    tiles[source.y * dimensions.width + source.x]? = some .floor →
    tiles[target.y * dimensions.width + target.x]? = some .floor →
    FloorReachable tiles dimensions.width source target
deriving Repr

public protected def Map.ofTiles (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions.width source target) : Map :=
  .mk dimensions tiles tilesSize allFloorsConnected

public def Map.tileAt? (map : Map) (position : Position) : Option Tile :=
  if position.x < map.dimensions.width ∧ position.y < map.dimensions.height then
    map.tiles[position.y * map.dimensions.width + position.x]?
  else
    none

public theorem Map.tileAt?_ofTiles (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions.width source target)
    (position : Position)
    (inBounds : position.x < dimensions.width ∧ position.y < dimensions.height) :
    (Map.ofTiles dimensions tiles tilesSize allFloorsConnected).tileAt? position =
      tiles[position.y * dimensions.width + position.x]? := by
  simp [Map.tileAt?, Map.ofTiles, inBounds]

public theorem Map.tileAt?_ne_none_of_in_bounds (map : Map) (position : Position)
    (inBounds : position.x < map.dimensions.width ∧ position.y < map.dimensions.height) :
    map.tileAt? position ≠ none := by
  intro tileIsNone
  simp only [Map.tileAt?, inBounds] at tileIsNone
  have indexOutOfBounds := (getElem?_eq_none_iff map.tiles
    (position.y * map.dimensions.width + position.x)).mp tileIsNone
  apply indexOutOfBounds
  rw [map.tilesSize]
  calc
    position.y * map.dimensions.width + position.x <
        (position.y + 1) * map.dimensions.width := by
      rw [Nat.add_mul]
      simpa using Nat.add_lt_add_left inBounds.1 (position.y * map.dimensions.width)
    _ ≤ map.dimensions.height * map.dimensions.width :=
      Nat.mul_le_mul_right map.dimensions.width inBounds.2
    _ = map.dimensions.width * map.dimensions.height := Nat.mul_comm _ _

end Gimlight

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

public protected def Map.create (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions.width source target) : Map :=
  .mk dimensions tiles tilesSize allFloorsConnected

public def Map.tileAt (map : Map) (position : Position) : Tile :=
  if position.x < map.dimensions.width ∧ position.y < map.dimensions.height then
    map.tiles[position.y * map.dimensions.width + position.x]?.getD .wall
  else
    .wall

public theorem Map.tileAt_create (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions.width source target)
    (position : Position)
    (inBounds : position.x < dimensions.width ∧ position.y < dimensions.height) :
    (Map.create dimensions tiles tilesSize allFloorsConnected).tileAt position =
      tiles[position.y * dimensions.width + position.x]?.getD .wall := by
  simp [Map.tileAt, Map.create, inBounds]

public def Map.reachable (map : Map) (source target : Position) : Prop :=
  FloorReachable map.tiles map.dimensions.width source target

public theorem Map.tileAt_eq_wall_of_not_in_bounds (map : Map) (position : Position)
    (outOfBounds : map.dimensions.width ≤ position.x ∨ map.dimensions.height ≤ position.y) :
    map.tileAt position = .wall := by
  simp only [Map.tileAt]
  rw [if_neg]
  omega

end Gimlight

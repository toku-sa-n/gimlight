module

public import Gimlight.Logic.Position.Basic
public import Gimlight.Logic.Room
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
  private rooms : Array Room
  private firstRoomCenter : Position
  private firstRoomCenterInBounds :
    firstRoomCenter.x < dimensions.width ∧ firstRoomCenter.y < dimensions.height
  private firstRoomCenterFloor :
    tiles[firstRoomCenter.y * dimensions.width + firstRoomCenter.x]? = some .floor
  private allFloorsConnected : ∀ source target,
    tiles[source.y * dimensions.width + source.x]? = some .floor →
    tiles[target.y * dimensions.width + target.x]? = some .floor →
    FloorReachable tiles dimensions.width source target
deriving Repr

public protected def Map.create (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height) (rooms : Array Room)
    (firstRoomCenter : Position)
    (firstRoomCenterInBounds :
      firstRoomCenter.x < dimensions.width ∧ firstRoomCenter.y < dimensions.height)
    (firstRoomCenterFloor :
      tiles[firstRoomCenter.y * dimensions.width + firstRoomCenter.x]? = some .floor)
    (allFloorsConnected : ∀ source target,
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions.width source target) : Map :=
  .mk dimensions tiles tilesSize rooms firstRoomCenter firstRoomCenterInBounds
    firstRoomCenterFloor allFloorsConnected

-- Convert a two-dimensional position to its row-major index.
private def Map.index (map : Map) (position : Position) : Option Nat :=
  if position.x < map.dimensions.width ∧ position.y < map.dimensions.height then
    some (position.y * map.dimensions.width + position.x)
  else
    none

public def Map.tileAt (map : Map) (position : Position) : Tile :=
  match map.index position with
  | some index => map.tiles[index]?.getD .wall
  | none => .wall

public def Map.roomList (map : Map) : Array Room := map.rooms

public def Map.start (map : Map) : Position := map.firstRoomCenter

public def Map.reachable (map : Map) (source target : Position) : Prop :=
  FloorReachable map.tiles map.dimensions.width source target

public theorem Map.start_is_floor (map : Map) : map.tileAt map.start = .floor := by
  simp only [Map.tileAt, Map.index, Map.start, if_pos map.firstRoomCenterInBounds]
  rw [map.firstRoomCenterFloor]
  rfl

public theorem Map.tileAt_eq_wall_of_not_in_bounds (map : Map) (position : Position)
    (outOfBounds : map.dimensions.width ≤ position.x ∨ map.dimensions.height ≤ position.y) :
    map.tileAt position = .wall := by
  simp only [Map.tileAt, Map.index]
  rw [if_neg]
  omega

end Gimlight

module

public import Gimlight.Logic.Position.Basic
public import Gimlight.Logic.Dimensions

namespace Gimlight

public inductive Tile where
  | wall
  | floor
deriving DecidableEq, Repr

public theorem Position.step_opposite {source target : Position} {direction : Direction}
    (stepped : source.step direction = some target) :
    target.step direction.opposite = some source := by
  rcases source with ⟨sourceX, sourceY⟩
  rcases target with ⟨targetX, targetY⟩
  cases direction <;> simp [Position.step, Direction.opposite] at stepped ⊢ <;> omega

public inductive FloorReachable (tiles : Array Tile) (dimensions : Dimensions)
    (source : Position) : Position → Prop where
  | refl
      (sourceInBounds : source.x < dimensions.width ∧ source.y < dimensions.height)
      (sourceFloor : tiles[source.y * dimensions.width + source.x]? = some .floor) :
      FloorReachable tiles dimensions source source
  | step {current target : Position}
      (path : FloorReachable tiles dimensions source current)
      (direction : Direction)
      (stepped : current.step direction = some target)
      (targetInBounds : target.x < dimensions.width ∧ target.y < dimensions.height)
      (targetFloor : tiles[target.y * dimensions.width + target.x]? = some .floor) :
      FloorReachable tiles dimensions source target

public theorem FloorReachable.sourceInBounds
    (reachable : FloorReachable tiles dimensions source target) :
    source.x < dimensions.width ∧ source.y < dimensions.height := by
  induction reachable with
  | refl sourceInBounds _ => exact sourceInBounds
  | step _ _ _ _ _ prefixInBounds => exact prefixInBounds

public theorem FloorReachable.sourceFloor
    (reachable : FloorReachable tiles dimensions source target) :
    tiles[source.y * dimensions.width + source.x]? = some .floor := by
  induction reachable with
  | refl _ sourceFloor => exact sourceFloor
  | step _ _ _ _ _ prefixFloor => exact prefixFloor

public theorem FloorReachable.targetInBounds
    (reachable : FloorReachable tiles dimensions source target) :
    target.x < dimensions.width ∧ target.y < dimensions.height := by
  cases reachable with
  | refl sourceInBounds _ => exact sourceInBounds
  | step _ _ _ targetInBounds _ => exact targetInBounds

public theorem FloorReachable.targetFloor
    (reachable : FloorReachable tiles dimensions source target) :
    tiles[target.y * dimensions.width + target.x]? = some .floor := by
  cases reachable with
  | refl _ sourceFloor => exact sourceFloor
  | step _ _ _ _ targetFloor => exact targetFloor

public theorem FloorReachable.mono
    (reachable : FloorReachable tiles dimensions source target)
    (preservesFloor : ∀ position : Position,
      position.x < dimensions.width ∧ position.y < dimensions.height →
      tiles[position.y * dimensions.width + position.x]? = some .floor →
      updated[position.y * dimensions.width + position.x]? = some .floor) :
    FloorReachable updated dimensions source target := by
  induction reachable with
  | refl sourceInBounds sourceFloor =>
      exact .refl sourceInBounds (preservesFloor _ sourceInBounds sourceFloor)
  | step path direction stepped targetInBounds targetFloor updatedPath =>
      exact .step updatedPath direction stepped targetInBounds
        (preservesFloor _ targetInBounds targetFloor)

public theorem FloorReachable.trans
    (first : FloorReachable tiles dimensions source middle)
    (second : FloorReachable tiles dimensions middle target) :
    FloorReachable tiles dimensions source target := by
  induction second with
  | refl _ _ => exact first
  | step _ direction stepped targetInBounds targetFloor connectedPrefix =>
      exact .step connectedPrefix direction stepped targetInBounds targetFloor

public theorem FloorReachable.symm
    (reachable : FloorReachable tiles dimensions source target) :
    FloorReachable tiles dimensions target source := by
  induction reachable with
  | refl sourceInBounds sourceFloor => exact .refl sourceInBounds sourceFloor
  | step path direction stepped targetInBounds targetFloor reversedPath =>
      have reversedStep : FloorReachable tiles dimensions _ _ :=
        .step (.refl targetInBounds targetFloor) direction.opposite
          (Position.step_opposite stepped)
          path.targetInBounds path.targetFloor
      exact reversedStep.trans reversedPath

-- This regression theorem prevents reachability from degenerating into checks on only its endpoints.
public theorem floor_wall_floor_not_reachable :
    ¬ FloorReachable #[.floor, .wall, .floor] { width := 3, height := 1 }
      { x := 0, y := 0 } { x := 2, y := 0 } := by
  intro reachable
  have onlySource : ∀ target,
      FloorReachable #[.floor, .wall, .floor] { width := 3, height := 1 }
        { x := 0, y := 0 } target → target = { x := 0, y := 0 } := by
    intro target path
    induction path with
    | refl => rfl
    | @step current next path direction stepped nextInBounds nextFloor pathEndsAtSource =>
        rw [pathEndsAtSource] at stepped
        cases direction with
        | left => simp [Position.step] at stepped
        | right =>
            simp [Position.step] at stepped
            subst next
            simp at nextFloor
        | up => simp [Position.step] at stepped
        | down =>
            simp [Position.step] at stepped
            subst next
            simp at nextInBounds
  have := onlySource _ reachable
  simp at this

public structure Map where private mk ::
  public dimensions : Dimensions
  -- Tiles are stored in row-major order at `y * dimensions.width + x`. A flat array lets
  -- `tiles.size = dimensions.width * dimensions.height` guarantee a rectangular map, without ragged
  -- rows or separate length proofs for every row as with `Array (Array Tile)`.
  private tiles : Array Tile
  private tilesSize : tiles.size = dimensions.width * dimensions.height
  private allFloorsConnected : ∀ source target,
    source.x < dimensions.width ∧ source.y < dimensions.height →
    target.x < dimensions.width ∧ target.y < dimensions.height →
    tiles[source.y * dimensions.width + source.x]? = some .floor →
    tiles[target.y * dimensions.width + target.x]? = some .floor →
    FloorReachable tiles dimensions source target
deriving Repr

public protected def Map.ofTiles (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      source.x < dimensions.width ∧ source.y < dimensions.height →
      target.x < dimensions.width ∧ target.y < dimensions.height →
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions source target) : Map :=
  .mk dimensions tiles tilesSize allFloorsConnected

public def Map.reachable (map : Map) (source target : Position) : Prop :=
  FloorReachable map.tiles map.dimensions source target

public def Map.tileAt? (map : Map) (position : Position) : Option Tile :=
  if position.x < map.dimensions.width ∧ position.y < map.dimensions.height then
    map.tiles[position.y * map.dimensions.width + position.x]?
  else
    none

public theorem Map.tileAt?_ofTiles (dimensions : Dimensions) (tiles : Array Tile)
    (tilesSize : tiles.size = dimensions.width * dimensions.height)
    (allFloorsConnected : ∀ source target,
      source.x < dimensions.width ∧ source.y < dimensions.height →
      target.x < dimensions.width ∧ target.y < dimensions.height →
      tiles[source.y * dimensions.width + source.x]? = some .floor →
      tiles[target.y * dimensions.width + target.x]? = some .floor →
      FloorReachable tiles dimensions source target)
    (position : Position)
    (inBounds : position.x < dimensions.width ∧ position.y < dimensions.height) :
    (Map.ofTiles dimensions tiles tilesSize allFloorsConnected).tileAt? position =
      tiles[position.y * dimensions.width + position.x]? := by
  simp [Map.tileAt?, Map.ofTiles, inBounds]

public theorem Map.floor_reachable (map : Map) (source target : Position)
    (sourceFloor : map.tileAt? source = some .floor)
    (targetFloor : map.tileAt? target = some .floor) :
    map.reachable source target := by
  have sourceInBounds : source.x < map.dimensions.width ∧
      source.y < map.dimensions.height := by
    simp only [Map.tileAt?] at sourceFloor
    split at sourceFloor
    · assumption
    · contradiction
  have targetInBounds : target.x < map.dimensions.width ∧
      target.y < map.dimensions.height := by
    simp only [Map.tileAt?] at targetFloor
    split at targetFloor
    · assumption
    · contradiction
  apply map.allFloorsConnected source target sourceInBounds targetInBounds
  · simpa [Map.tileAt?, sourceInBounds] using sourceFloor
  · simpa [Map.tileAt?, targetInBounds] using targetFloor

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

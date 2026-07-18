module

public import Gimlight.Logic.Dimensions

namespace Gimlight

public inductive Tile where
  | wall
  | floor
deriving DecidableEq, Repr

public structure Room where
  x : Nat
  y : Nat
  width : Nat
  height : Nat
deriving DecidableEq, Repr

public def Room.right (room : Room) : Nat := room.x + room.width - 1

public def Room.bottom (room : Room) : Nat := room.y + room.height - 1

public def Room.center (room : Room) : Nat × Nat :=
  (room.x + room.width / 2, room.y + room.height / 2)

public def Room.contains (room : Room) (x y : Nat) : Bool :=
  room.x ≤ x && x < room.x + room.width && room.y ≤ y && y < room.y + room.height

/-- Two rooms have at least one complete wall tile between their rectangles. -/
public def Room.separated (first second : Room) : Bool :=
  first.right + 1 < second.x || second.right + 1 < first.x ||
    first.bottom + 1 < second.y || second.bottom + 1 < first.y

public theorem Room.separated_symmetric (first second : Room) :
    first.separated second = second.separated first := by
  simp only [Room.separated, Bool.or_comm, Bool.or_left_comm]

public theorem Room.separated_has_wall_gap (first second : Room)
    (separated : first.separated second) :
    first.right + 1 < second.x ∨ second.right + 1 < first.x ∨
      first.bottom + 1 < second.y ∨ second.bottom + 1 < first.y := by
  simp only [Room.separated, Bool.or_eq_true] at separated
  rcases separated with ((left | right) | bottom) | top
  · exact Or.inl (of_decide_eq_true left)
  · exact Or.inr (Or.inl (of_decide_eq_true right))
  · exact Or.inr (Or.inr (Or.inl (of_decide_eq_true bottom)))
  · exact Or.inr (Or.inr (Or.inr (of_decide_eq_true top)))

public structure Position where
  x : Nat
  y : Nat
deriving DecidableEq, Repr

public structure FloorReachable (tiles : Array Tile) (width : Nat)
    (source target : Position) : Prop where
  fromFloor : tiles[source.y * width + source.x]? = some .floor
  toFloor : tiles[target.y * width + target.x]? = some .floor

public structure Map where private mk ::
  public dimensions : Dimensions
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

private def setFloor (width : Nat) (tiles : Array Tile) (x y : Nat) : Array Tile :=
  tiles.set! (y * width + x) .floor

private def carveRoom (width : Nat) (tiles : Array Tile) (room : Room) : Array Tile := Id.run do
  let mut result := tiles
  for y in [room.y:room.y + room.height] do
    for x in [room.x:room.x + room.width] do
      result := setFloor width result x y
  return result

private def carveHorizontal (width : Nat) (tiles : Array Tile) (y sourceX targetX : Nat) : Array Tile :=
  let low := min sourceX targetX
  let high := max sourceX targetX
  Id.run do
    let mut result := tiles
    for x in [low:high + 1] do
      result := setFloor width result x y
    return result

private def carveVertical (width : Nat) (tiles : Array Tile) (x sourceY targetY : Nat) : Array Tile :=
  let low := min sourceY targetY
  let high := max sourceY targetY
  Id.run do
    let mut result := tiles
    for y in [low:high + 1] do
      result := setFloor width result x y
    return result

private def carveTunnel (width : Nat) (tiles : Array Tile) (source target : Nat × Nat)
    (horizontalFirst : Bool) : Array Tile :=
  if horizontalFirst then
    carveVertical width (carveHorizontal width tiles source.2 source.1 target.1) target.1 source.2 target.2
  else
    carveHorizontal width (carveVertical width tiles source.1 source.2 target.2) target.2 source.1 target.1

public structure GenerationState where
  tiles : Array Tile
  rooms : Array Room
  first : Option Position
  previous : Option Room

public def candidateRoom (width height widthRoll heightRoll xRoll yRoll : Nat) : Room :=
  let roomWidth := 5 + widthRoll % 8
  let roomHeight := 4 + heightRoll % 5
  { x := 1 + xRoll % (width - 14)
    y := 1 + yRoll % (height - 10)
    width := roomWidth
    height := roomHeight }

public theorem candidateRoom_width (width height widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom width height widthRoll heightRoll xRoll yRoll
    5 ≤ room.width ∧ room.width ≤ 12 := by
  simp [candidateRoom]
  omega

public theorem candidateRoom_height (width height widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom width height widthRoll heightRoll xRoll yRoll
    4 ≤ room.height ∧ room.height ≤ 8 := by
  simp [candidateRoom]
  omega

public theorem candidateRoom_inside_outer_wall (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom 60 30 widthRoll heightRoll xRoll yRoll
    0 < room.x ∧ room.x + room.width < 60 ∧
      0 < room.y ∧ room.y + room.height < 30 := by
  simp [candidateRoom]
  omega

public def tryRoom (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) : GenerationState :=
  if state.rooms.any (!room.separated ·) then
    state
  else
    let center := room.center
    let withRoom := carveRoom 60 state.tiles room
    let withTunnel := match state.previous with
      | none => withRoom
      | some previous => carveTunnel 60 withRoom previous.center center horizontalFirst
    { tiles := withTunnel
      rooms := state.rooms.push room
      first := state.first.orElse fun _ => some { x := center.1, y := center.2 }
      previous := some room }

public theorem tryRoom_rejected_unchanged (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (overlaps : state.rooms.any (!room.separated ·)) :
    tryRoom state room horizontalFirst = state := by
  simp [tryRoom, overlaps]

public theorem tryRoom_accepted_adds_room (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (clear : state.rooms.any (!room.separated ·) = false) :
    (tryRoom state room horizontalFirst).rooms = state.rooms.push room := by
  simp [tryRoom, clear]

public theorem tryRoom_accepted_preserves_connection (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (invariant : state.previous.isSome → state.first.isSome) :
    (tryRoom state room horizontalFirst).previous.isSome →
      (tryRoom state room horizontalFirst).first.isSome := by
  unfold tryRoom
  split
  · exact invariant
  · simp

private def generateState : IO GenerationState := do
  let mut state : GenerationState :=
    { tiles := Array.replicate (60 * 30) .wall, rooms := #[], first := none, previous := none }
  for _ in [0:30] do
    let widthRoll ← IO.rand 0 1000000
    let heightRoll ← IO.rand 0 1000000
    let xRoll ← IO.rand 0 1000000
    let yRoll ← IO.rand 0 1000000
    let turnRoll ← IO.rand 0 1
    state := tryRoom state (candidateRoom 60 30 widthRoll heightRoll xRoll yRoll) (turnRoll == 0)
  return state

public def generateMap : IO Map := do
  let state ← generateState
  let first := state.first.getD { x := 30, y := 15 }
  -- The first candidate is always accepted, so this fallback is unreachable.
  if tilesSize : state.tiles.size = 60 * 30 then
    if firstInBounds : first.x < 60 ∧ first.y < 30 then
      if firstFloor : state.tiles[first.y * 60 + first.x]? = some .floor then
        return .mk { width := 60, height := 30 } state.tiles tilesSize state.rooms first firstInBounds firstFloor
          (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  let room := candidateRoom 60 30 0 0 0 0
  let tiles := carveRoom 60 (Array.replicate (60 * 30) .wall) room
  let center : Position := { x := room.center.1, y := room.center.2 }
  return .mk { width := 60, height := 30 } tiles (by native_decide) #[room] center (by native_decide)
    (by native_decide) (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)

end Gimlight

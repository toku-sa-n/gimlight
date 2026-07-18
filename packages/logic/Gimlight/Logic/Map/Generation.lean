module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public meta import Gimlight.Logic.Map.Core
public meta import Gimlight.Logic.Room

namespace Gimlight

public structure GeneratedMap where private mk ::
  public map : Map
  public start : Position
  private startOnFloor : map.tileAt start = .floor

public theorem GeneratedMap.start_is_floor (generated : GeneratedMap) :
    generated.map.tileAt generated.start = .floor :=
  generated.startOnFloor

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

public def generateMap : IO GeneratedMap := do
  let state ← generateState
  let first := state.first.getD { x := 30, y := 15 }
  -- The first candidate is always accepted, so this fallback is unreachable.
  if tilesSize : state.tiles.size = 60 * 30 then
    if firstInBounds : first.x < 60 ∧ first.y < 30 then
      if firstFloor : state.tiles[first.y * 60 + first.x]? = some .floor then
        let map := Map.ofTiles { width := 60, height := 30 } state.tiles tilesSize
          (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
        return .mk map first (by simp [map, Map.tileAt_ofTiles, firstInBounds, firstFloor])
  let room := candidateRoom 60 30 0 0 0 0
  let tiles := carveRoom 60 (Array.replicate (60 * 30) .wall) room
  let center : Position := { x := room.center.1, y := room.center.2 }
  let map := Map.ofTiles { width := 60, height := 30 } tiles (by native_decide)
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map center (by native_decide)

end Gimlight

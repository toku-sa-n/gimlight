module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public meta import Gimlight.Logic.Map.Core
public meta import Gimlight.Logic.Room

namespace Gimlight

private def mapWidth : Nat := 60
private def mapHeight : Nat := 30
private def mapArea : Nat := mapWidth * mapHeight
private def minRoomWidth : Nat := 5
private def maxRoomWidth : Nat := 12
private def minRoomHeight : Nat := 4
private def maxRoomHeight : Nat := 8
private def roomGenerationAttempts : Nat := 30
private def randomUpperBound : Nat := 1000000
private def outerWallMargin : Nat := 1

private def roomWidthRange : Nat := maxRoomWidth - minRoomWidth + 1
private def roomHeightRange : Nat := maxRoomHeight - minRoomHeight + 1
private def roomXRange (width : Nat) : Nat := width - (maxRoomWidth + outerWallMargin * 2)
private def roomYRange (height : Nat) : Nat := height - (maxRoomHeight + outerWallMargin * 2)

public structure GeneratedMap where private mk ::
  public map : Map
  public start : Position
  public startOnFloor : map.tileAt? start = some .floor

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
  let roomWidth := minRoomWidth + widthRoll % roomWidthRange
  let roomHeight := minRoomHeight + heightRoll % roomHeightRange
  { x := outerWallMargin + xRoll % roomXRange width
    y := outerWallMargin + yRoll % roomYRange height
    width := roomWidth
    height := roomHeight }

public theorem candidateRoom_width (width height widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom width height widthRoll heightRoll xRoll yRoll
    5 ≤ room.width ∧ room.width ≤ 12 := by
  simp [candidateRoom, minRoomWidth, maxRoomWidth, roomWidthRange]
  omega

public theorem candidateRoom_height (width height widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom width height widthRoll heightRoll xRoll yRoll
    4 ≤ room.height ∧ room.height ≤ 8 := by
  simp [candidateRoom, minRoomHeight, maxRoomHeight, roomHeightRange]
  omega

public theorem candidateRoom_inside_outer_wall (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom 60 30 widthRoll heightRoll xRoll yRoll
    0 < room.x ∧ room.x + room.width < 60 ∧
      0 < room.y ∧ room.y + room.height < 30 := by
  simp [candidateRoom, minRoomWidth, maxRoomWidth, minRoomHeight, maxRoomHeight,
    roomWidthRange, roomHeightRange, outerWallMargin, roomXRange, roomYRange]
  omega

public def tryRoom (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) : GenerationState :=
  if state.rooms.any (!room.separated ·) then
    state
  else
    let center := room.center
    let withRoom := carveRoom mapWidth state.tiles room
    let withTunnel := match state.previous with
      | none => withRoom
      | some previous => carveTunnel mapWidth withRoom previous.center center horizontalFirst
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

private def randomRoom : IO Room := do
  let widthRoll ← IO.rand 0 randomUpperBound
  let heightRoll ← IO.rand 0 randomUpperBound
  let xRoll ← IO.rand 0 randomUpperBound
  let yRoll ← IO.rand 0 randomUpperBound
  return candidateRoom mapWidth mapHeight widthRoll heightRoll xRoll yRoll

private def generateState : IO GenerationState := do
  let mut state : GenerationState :=
    { tiles := Array.replicate mapArea .wall, rooms := #[], first := none, previous := none }
  for _ in [0:roomGenerationAttempts] do
    let room ← randomRoom
    let turnRoll ← IO.rand 0 1
    state := tryRoom state room (turnRoll == 0)
  return state

public def generateMap : IO GeneratedMap := do
  let state ← generateState
  let first := state.first.getD { x := mapWidth / 2, y := mapHeight / 2 }
  -- The first candidate is always accepted, so this fallback is unreachable.
  if tilesSize : state.tiles.size = mapArea then
    if firstInBounds : first.x < mapWidth ∧ first.y < mapHeight then
      if firstFloor : state.tiles[first.y * mapWidth + first.x]? = some .floor then
        let map := Map.ofTiles { width := mapWidth, height := mapHeight } state.tiles tilesSize
          (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
        return .mk map first (by simp [map, Map.tileAt?_ofTiles, firstInBounds, firstFloor])
  let room := candidateRoom mapWidth mapHeight 0 0 0 0
  let tiles := carveRoom mapWidth (Array.replicate mapArea .wall) room
  let center : Position := { x := room.center.1, y := room.center.2 }
  let map := Map.ofTiles { width := mapWidth, height := mapHeight } tiles (by native_decide)
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map center (by native_decide)

end Gimlight

module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public meta import Gimlight.Logic.Map.Core
public meta import Gimlight.Logic.Room

namespace Gimlight

private def randomUpperBound : Nat := 1000000

public structure MapGenerationParameters where private mk ::
  public width : Nat
  public height : Nat
  public minRoomWidth : Nat
  public maxRoomWidth : Nat
  public minRoomHeight : Nat
  public maxRoomHeight : Nat
  public outerWallMargin : Nat
  public attempts : Nat
  private minRoomWidthPositive : 0 < minRoomWidth
  private roomWidthOrdered : minRoomWidth ≤ maxRoomWidth
  private minRoomHeightPositive : 0 < minRoomHeight
  private roomHeightOrdered : minRoomHeight ≤ maxRoomHeight
  private roomWidthFits : maxRoomWidth + outerWallMargin * 2 < width
  private roomHeightFits : maxRoomHeight + outerWallMargin * 2 < height
  private attemptsPositive : 0 < attempts

public def MapGenerationParameters.create? (width height minRoomWidth maxRoomWidth
    minRoomHeight maxRoomHeight outerWallMargin attempts : Nat) : Option MapGenerationParameters :=
  if minRoomWidthPositive : 0 < minRoomWidth then
    if roomWidthOrdered : minRoomWidth ≤ maxRoomWidth then
      if minRoomHeightPositive : 0 < minRoomHeight then
        if roomHeightOrdered : minRoomHeight ≤ maxRoomHeight then
          if roomWidthFits : maxRoomWidth + outerWallMargin * 2 < width then
            if roomHeightFits : maxRoomHeight + outerWallMargin * 2 < height then
              if attemptsPositive : 0 < attempts then
                some (.mk width height minRoomWidth maxRoomWidth minRoomHeight maxRoomHeight
                  outerWallMargin attempts minRoomWidthPositive roomWidthOrdered
                  minRoomHeightPositive roomHeightOrdered roomWidthFits roomHeightFits attemptsPositive)
              else none
            else none
          else none
        else none
      else none
    else none
  else none

public def MapGenerationParameters.default : MapGenerationParameters :=
  .mk 60 30 5 12 4 8 1 30 (by decide) (by decide) (by decide) (by decide) (by decide)
    (by decide) (by decide)

private def MapGenerationParameters.mapArea (parameters : MapGenerationParameters) : Nat :=
  parameters.width * parameters.height

private def MapGenerationParameters.roomWidthRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomWidth - parameters.minRoomWidth + 1

private def MapGenerationParameters.roomHeightRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomHeight - parameters.minRoomHeight + 1

private def MapGenerationParameters.roomXRange (parameters : MapGenerationParameters) : Nat :=
  parameters.width - (parameters.maxRoomWidth + parameters.outerWallMargin * 2)

private def MapGenerationParameters.roomYRange (parameters : MapGenerationParameters) : Nat :=
  parameters.height - (parameters.maxRoomHeight + parameters.outerWallMargin * 2)

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

public def candidateRoom (parameters : MapGenerationParameters)
    (widthRoll heightRoll xRoll yRoll : Nat) : Room :=
  let roomWidth := parameters.minRoomWidth + widthRoll % parameters.roomWidthRange
  let roomHeight := parameters.minRoomHeight + heightRoll % parameters.roomHeightRange
  { x := parameters.outerWallMargin + xRoll % parameters.roomXRange
    y := parameters.outerWallMargin + yRoll % parameters.roomYRange
    width := roomWidth
    height := roomHeight }

public theorem candidateRoom_width (parameters : MapGenerationParameters)
    (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.minRoomWidth ≤ room.width ∧ room.width ≤ parameters.maxRoomWidth := by
  simp [candidateRoom, MapGenerationParameters.roomWidthRange]
  have := parameters.roomWidthOrdered
  have rangePositive : 0 < parameters.maxRoomWidth - parameters.minRoomWidth + 1 := by omega
  have rollInRange := Nat.mod_lt widthRoll rangePositive
  omega

public theorem candidateRoom_height (parameters : MapGenerationParameters)
    (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.minRoomHeight ≤ room.height ∧ room.height ≤ parameters.maxRoomHeight := by
  simp [candidateRoom, MapGenerationParameters.roomHeightRange]
  have := parameters.roomHeightOrdered
  have rangePositive : 0 < parameters.maxRoomHeight - parameters.minRoomHeight + 1 := by omega
  have rollInRange := Nat.mod_lt heightRoll rangePositive
  omega

public theorem candidateRoom_inside_outer_wall (parameters : MapGenerationParameters)
    (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.outerWallMargin ≤ room.x ∧
      room.x + room.width + parameters.outerWallMargin < parameters.width ∧
      parameters.outerWallMargin ≤ room.y ∧
      room.y + room.height + parameters.outerWallMargin < parameters.height := by
  have widthFits := parameters.roomWidthFits
  have heightFits := parameters.roomHeightFits
  have xRangePositive : 0 < parameters.roomXRange := by
    simp [MapGenerationParameters.roomXRange]
    omega
  have yRangePositive : 0 < parameters.roomYRange := by
    simp [MapGenerationParameters.roomYRange]
    omega
  have xRollInRange := Nat.mod_lt xRoll xRangePositive
  have yRollInRange := Nat.mod_lt yRoll yRangePositive
  have roomWidthInRange := candidateRoom_width parameters widthRoll heightRoll xRoll yRoll
  have roomHeightInRange := candidateRoom_height parameters widthRoll heightRoll xRoll yRoll
  simp [candidateRoom, MapGenerationParameters.roomXRange,
    MapGenerationParameters.roomYRange] at *
  omega

public theorem default_candidateRoom_inside_outer_wall (widthRoll heightRoll xRoll yRoll : Nat) :
    let room := candidateRoom .default widthRoll heightRoll xRoll yRoll
    0 < room.x ∧ room.x + room.width < 60 ∧
      0 < room.y ∧ room.y + room.height < 30 := by
  have inside := candidateRoom_inside_outer_wall .default widthRoll heightRoll xRoll yRoll
  simp [MapGenerationParameters.default] at inside ⊢
  omega

public def tryRoom (parameters : MapGenerationParameters) (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) : GenerationState :=
  if state.rooms.any (!room.separated ·) then
    state
  else
    let center := room.center
    let withRoom := carveRoom parameters.width state.tiles room
    let withTunnel := match state.previous with
      | none => withRoom
      | some previous => carveTunnel parameters.width withRoom previous.center center horizontalFirst
    { tiles := withTunnel
      rooms := state.rooms.push room
      first := state.first.orElse fun _ => some { x := center.1, y := center.2 }
      previous := some room }

public theorem tryRoom_rejected_unchanged (parameters : MapGenerationParameters)
    (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (overlaps : state.rooms.any (!room.separated ·)) :
    tryRoom parameters state room horizontalFirst = state := by
  simp [tryRoom, overlaps]

public theorem tryRoom_accepted_adds_room (parameters : MapGenerationParameters)
    (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (clear : state.rooms.any (!room.separated ·) = false) :
    (tryRoom parameters state room horizontalFirst).rooms = state.rooms.push room := by
  simp [tryRoom, clear]

public theorem tryRoom_accepted_preserves_connection (parameters : MapGenerationParameters)
    (state : GenerationState) (room : Room)
    (horizontalFirst : Bool) (invariant : state.previous.isSome → state.first.isSome) :
    (tryRoom parameters state room horizontalFirst).previous.isSome →
      (tryRoom parameters state room horizontalFirst).first.isSome := by
  unfold tryRoom
  split
  · exact invariant
  · simp

private def randomRoom (parameters : MapGenerationParameters) : IO Room := do
  let widthRoll ← IO.rand 0 randomUpperBound
  let heightRoll ← IO.rand 0 randomUpperBound
  let xRoll ← IO.rand 0 randomUpperBound
  let yRoll ← IO.rand 0 randomUpperBound
  return candidateRoom parameters widthRoll heightRoll xRoll yRoll

private def generateState (parameters : MapGenerationParameters) : IO GenerationState := do
  let mut state : GenerationState :=
    { tiles := Array.replicate parameters.mapArea .wall, rooms := #[], first := none, previous := none }
  for _ in [0:parameters.attempts] do
    let room ← randomRoom parameters
    let turnRoll ← IO.rand 0 1
    state := tryRoom parameters state room (turnRoll == 0)
  return state

public def generateMap (parameters : MapGenerationParameters) : IO GeneratedMap := do
  let state ← generateState parameters
  let first := state.first.getD { x := parameters.width / 2, y := parameters.height / 2 }
  -- The first candidate is always accepted, so this fallback is unreachable.
  if tilesSize : state.tiles.size = parameters.mapArea then
    if firstInBounds : first.x < parameters.width ∧ first.y < parameters.height then
      if firstFloor : state.tiles[first.y * parameters.width + first.x]? = some .floor then
        let map := Map.ofTiles { width := parameters.width, height := parameters.height } state.tiles tilesSize
          (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
        return .mk map first (by simp [map, Map.tileAt?_ofTiles, firstInBounds, firstFloor])
  let tiles := (Array.replicate parameters.mapArea .wall).set! 0 .floor
  let center : Position := { x := 0, y := 0 }
  let map := Map.ofTiles { width := parameters.width, height := parameters.height } tiles (by
      dsimp [tiles]
      rw [Array.size_setIfInBounds]
      simp [MapGenerationParameters.mapArea])
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map center (by
    have widthPositive : 0 < parameters.width := by
      have := parameters.roomWidthFits
      omega
    have heightPositive : 0 < parameters.height := by
      have := parameters.roomHeightFits
      omega
    simp [map, Map.tileAt?_ofTiles, center, widthPositive, heightPositive, tiles,
      MapGenerationParameters.mapArea])

public theorem MapGenerationParameters.create?_rejects_invalid_width_range :
    MapGenerationParameters.create? 60 30 12 5 4 8 1 30 = none := by decide

public theorem MapGenerationParameters.create?_rejects_invalid_height_range :
    MapGenerationParameters.create? 60 30 5 12 8 4 1 30 = none := by decide

public theorem MapGenerationParameters.create?_rejects_small_map :
    MapGenerationParameters.create? 14 10 5 12 4 8 1 30 = none := by decide

public theorem MapGenerationParameters.create?_rejects_zero_room_dimension :
    MapGenerationParameters.create? 60 30 0 12 4 8 1 30 = none := by decide

public theorem MapGenerationParameters.create?_rejects_zero_room_height :
    MapGenerationParameters.create? 60 30 5 12 0 8 1 30 = none := by decide

public theorem MapGenerationParameters.create?_rejects_zero_attempts :
    MapGenerationParameters.create? 60 30 5 12 4 8 1 0 = none := by decide

end Gimlight

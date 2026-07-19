module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
import Gimlight.Logic.Random
public meta import Gimlight.Logic.Map.Core
public meta import Gimlight.Logic.Room

namespace Gimlight

private def MapGenerationParameters.mapArea (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.width * parameters.dimensions.height

private def MapGenerationParameters.roomWidthRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomDimensions.width - parameters.minRoomDimensions.width + 1

private def MapGenerationParameters.roomHeightRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomDimensions.height - parameters.minRoomDimensions.height + 1

private def MapGenerationParameters.roomXRange (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.width - (parameters.maxRoomDimensions.width + parameters.outerWallMargin * 2)

private def MapGenerationParameters.roomYRange (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.height - (parameters.maxRoomDimensions.height + parameters.outerWallMargin * 2)

public structure GeneratedMap where private mk ::
  public map : Map
  public start : Position
  public startOnFloor : map.tileAt? start = some .floor

private def setFloor (width : Nat) (tiles : Array Tile) (x y : Nat) : Array Tile :=
  tiles.set! (y * width + x) .floor

private def carveRoom (width : Nat) (tiles : Array Tile) (room : Room) : Array Tile :=
  (List.range' room.y room.height).foldl (fun result y =>
    (List.range' room.x room.width).foldl (fun result x => setFloor width result x y) result)
    tiles

private def carveHorizontal (width : Nat) (tiles : Array Tile) (y sourceX targetX : Nat) : Array Tile :=
  let low := min sourceX targetX
  let high := max sourceX targetX
  (List.range' low (high + 1 - low)).foldl (fun result x => setFloor width result x y) tiles

private def carveVertical (width : Nat) (tiles : Array Tile) (x sourceY targetY : Nat) : Array Tile :=
  let low := min sourceY targetY
  let high := max sourceY targetY
  (List.range' low (high + 1 - low)).foldl (fun result y => setFloor width result x y) tiles

private def carveTunnel (width : Nat) (tiles : Array Tile) (source target : Position)
    (horizontalFirst : Bool) : Array Tile :=
  if horizontalFirst then
    carveVertical width (carveHorizontal width tiles source.y source.x target.x) target.x source.y target.y
  else
    carveHorizontal width (carveVertical width tiles source.x source.y target.y) target.y source.x target.x

private theorem setFloor_size (width : Nat) (tiles : Array Tile) (x y : Nat) :
    (setFloor width tiles x y).size = tiles.size := by
  simp [setFloor, Array.size_setIfInBounds]

private theorem foldl_size (items : List α) (initial : Array Tile)
    (operation : Array Tile → α → Array Tile)
    (preservesSize : ∀ tiles item, (operation tiles item).size = tiles.size) :
    (items.foldl operation initial).size = initial.size := by
  induction items generalizing initial with
  | nil => rfl
  | cons item items induction =>
      simp only [List.foldl_cons]
      rw [induction, preservesSize]

private theorem carveRoom_size (width : Nat) (tiles : Array Tile) (room : Room) :
    (carveRoom width tiles room).size = tiles.size := by
  simp only [carveRoom, setFloor]
  apply foldl_size
  intro row y
  apply foldl_size
  simp

private theorem carveHorizontal_size (width : Nat) (tiles : Array Tile)
    (y sourceX targetX : Nat) :
    (carveHorizontal width tiles y sourceX targetX).size = tiles.size := by
  simp only [carveHorizontal, setFloor]
  apply foldl_size
  simp

private theorem carveVertical_size (width : Nat) (tiles : Array Tile)
    (x sourceY targetY : Nat) :
    (carveVertical width tiles x sourceY targetY).size = tiles.size := by
  simp only [carveVertical, setFloor]
  apply foldl_size
  simp

private theorem carveTunnel_size (width : Nat) (tiles : Array Tile) (source target : Position)
    (horizontalFirst : Bool) :
    (carveTunnel width tiles source target horizontalFirst).size = tiles.size := by
  cases horizontalFirst <;>
    simp [carveTunnel, carveHorizontal_size, carveVertical_size]

private structure GenerationState (parameters : MapGenerationParameters) where
  tiles : Array Tile
  tilesSize : tiles.size = parameters.mapArea
  rooms : Array Room
  first : Position
  firstInBounds : first.x < parameters.dimensions.width ∧
    first.y < parameters.dimensions.height
  firstFloor : tiles[first.y * parameters.dimensions.width + first.x]? = some .floor
  previous : Room

private def candidateRoom (parameters : MapGenerationParameters)
    (widthRoll : Fin parameters.roomWidthRange)
    (heightRoll : Fin parameters.roomHeightRange)
    (xRoll : Fin parameters.roomXRange)
    (yRoll : Fin parameters.roomYRange) : Room :=
  let roomWidth := parameters.minRoomDimensions.width + widthRoll.val
  let roomHeight := parameters.minRoomDimensions.height + heightRoll.val
  { x := parameters.outerWallMargin + xRoll.val
    y := parameters.outerWallMargin + yRoll.val
    width := roomWidth
    height := roomHeight }

private theorem candidateRoom_width (parameters : MapGenerationParameters)
    (widthRoll : Fin parameters.roomWidthRange)
    (heightRoll : Fin parameters.roomHeightRange)
    (xRoll : Fin parameters.roomXRange)
    (yRoll : Fin parameters.roomYRange) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.minRoomDimensions.width ≤ room.width ∧
      room.width ≤ parameters.maxRoomDimensions.width := by
  simp [candidateRoom, MapGenerationParameters.roomWidthRange]
  obtain ⟨_, roomWidthOrdered, _, _, _, _, _⟩ := parameters.valid
  have rollInRange := widthRoll.isLt
  simp [MapGenerationParameters.roomWidthRange] at rollInRange
  omega

private theorem candidateRoom_height (parameters : MapGenerationParameters)
    (widthRoll : Fin parameters.roomWidthRange)
    (heightRoll : Fin parameters.roomHeightRange)
    (xRoll : Fin parameters.roomXRange)
    (yRoll : Fin parameters.roomYRange) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.minRoomDimensions.height ≤ room.height ∧
      room.height ≤ parameters.maxRoomDimensions.height := by
  simp [candidateRoom, MapGenerationParameters.roomHeightRange]
  obtain ⟨_, _, _, roomHeightOrdered, _, _, _⟩ := parameters.valid
  have rollInRange := heightRoll.isLt
  simp [MapGenerationParameters.roomHeightRange] at rollInRange
  omega

private theorem candidateRoom_inside_outer_wall (parameters : MapGenerationParameters)
    (widthRoll : Fin parameters.roomWidthRange)
    (heightRoll : Fin parameters.roomHeightRange)
    (xRoll : Fin parameters.roomXRange)
    (yRoll : Fin parameters.roomYRange) :
    let room := candidateRoom parameters widthRoll heightRoll xRoll yRoll
    parameters.outerWallMargin ≤ room.x ∧
      room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
      parameters.outerWallMargin ≤ room.y ∧
      room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height := by
  obtain ⟨_, _, _, _, widthFits, heightFits, _⟩ := parameters.valid
  have xRollInRange := xRoll.isLt
  have yRollInRange := yRoll.isLt
  have roomWidthInRange := candidateRoom_width parameters widthRoll heightRoll xRoll yRoll
  have roomHeightInRange := candidateRoom_height parameters widthRoll heightRoll xRoll yRoll
  simp [candidateRoom, MapGenerationParameters.roomXRange,
    MapGenerationParameters.roomYRange] at *
  omega

private theorem positionIndex_lt (parameters : MapGenerationParameters) (tiles : Array Tile)
    (tilesSize : tiles.size = parameters.mapArea) (position : Position)
    (inBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height) :
    position.y * parameters.dimensions.width + position.x < tiles.size := by
  rw [tilesSize]
  simp [MapGenerationParameters.mapArea]
  calc
    position.y * parameters.dimensions.width + position.x <
        (position.y + 1) * parameters.dimensions.width := by
      rw [Nat.add_mul]
      simpa using Nat.add_lt_add_left inBounds.1
        (position.y * parameters.dimensions.width)
    _ ≤ parameters.dimensions.height * parameters.dimensions.width :=
      Nat.mul_le_mul_right parameters.dimensions.width inBounds.2
    _ = parameters.dimensions.width * parameters.dimensions.height := Nat.mul_comm _ _

private theorem setFloor_at (width : Nat) (tiles : Array Tile) (position : Position)
    (inBounds : position.y * width + position.x < tiles.size) :
    (setFloor width tiles position.x position.y)[position.y * width + position.x]? =
      some .floor := by
  simp [setFloor, inBounds]

private def tryRoom (parameters : MapGenerationParameters) (state : GenerationState parameters)
    (room : Room) (horizontalFirst : Bool) : GenerationState parameters :=
  if state.rooms.any (!room.separated ·) then
    state
  else
    let center := room.center
    let withRoom := carveRoom parameters.dimensions.width state.tiles room
    let withTunnel := carveTunnel parameters.dimensions.width withRoom state.previous.center center
      horizontalFirst
    let tiles := setFloor parameters.dimensions.width withTunnel state.first.x state.first.y
    { tiles
      tilesSize := by
        simp [tiles, withTunnel, withRoom, setFloor_size, carveTunnel_size, carveRoom_size,
          state.tilesSize]
      rooms := state.rooms.push room
      first := state.first
      firstInBounds := state.firstInBounds
      firstFloor := setFloor_at parameters.dimensions.width withTunnel state.first
        (by
          apply positionIndex_lt parameters withTunnel
          · simp [withTunnel, withRoom, carveTunnel_size, carveRoom_size, state.tilesSize]
          · exact state.firstInBounds)
      previous := room }

private theorem tryRoom_rejected_unchanged (parameters : MapGenerationParameters)
    (state : GenerationState parameters) (room : Room)
    (horizontalFirst : Bool) (overlaps : state.rooms.any (!room.separated ·)) :
    tryRoom parameters state room horizontalFirst = state := by
  simp [tryRoom, overlaps]

private theorem tryRoom_accepted_adds_room (parameters : MapGenerationParameters)
    (state : GenerationState parameters) (room : Room)
    (horizontalFirst : Bool) (clear : state.rooms.any (!room.separated ·) = false) :
    (tryRoom parameters state room horizontalFirst).rooms = state.rooms.push room := by
  simp [tryRoom, clear]

private def randomRoom (parameters : MapGenerationParameters) : IO
    { room : Room // parameters.outerWallMargin ≤ room.x ∧
      room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
      parameters.outerWallMargin ≤ room.y ∧
      room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height } := do
  let widthRoll ← Random.fin parameters.roomWidthRange (by
    simp [MapGenerationParameters.roomWidthRange])
  let heightRoll ← Random.fin parameters.roomHeightRange (by
    simp [MapGenerationParameters.roomHeightRange])
  let xRoll ← Random.fin parameters.roomXRange (by
    simp [MapGenerationParameters.roomXRange]
    obtain ⟨_, _, _, _, widthFits, _, _⟩ := parameters.valid
    omega)
  let yRoll ← Random.fin parameters.roomYRange (by
    simp [MapGenerationParameters.roomYRange]
    obtain ⟨_, _, _, _, _, heightFits, _⟩ := parameters.valid
    omega)
  return ⟨candidateRoom parameters widthRoll heightRoll xRoll yRoll,
    candidateRoom_inside_outer_wall parameters widthRoll heightRoll xRoll yRoll⟩

private def initialState (parameters : MapGenerationParameters)
    (candidate : { room : Room // parameters.outerWallMargin ≤ room.x ∧
      room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
      parameters.outerWallMargin ≤ room.y ∧
      room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height }) :
    GenerationState parameters :=
  let room := candidate.1
  let center : Position := { x := room.x + room.width / 2, y := room.y + room.height / 2 }
  let base := Array.replicate parameters.mapArea .wall
  let carved := carveRoom parameters.dimensions.width base room
  let tiles := setFloor parameters.dimensions.width carved center.x center.y
  have centerInBounds : center.x < parameters.dimensions.width ∧
      center.y < parameters.dimensions.height := by
    have inside := candidate.2
    have widthHalf := Nat.div_le_self room.width 2
    have heightHalf := Nat.div_le_self room.height 2
    change room.x + room.width / 2 < parameters.dimensions.width ∧
      room.y + room.height / 2 < parameters.dimensions.height
    change parameters.outerWallMargin ≤ room.x ∧
      room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
      parameters.outerWallMargin ≤ room.y ∧
      room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height at inside
    omega
  { tiles
    tilesSize := by
      simp [tiles, carved, base, setFloor_size, carveRoom_size,
        MapGenerationParameters.mapArea]
    rooms := #[room]
    first := center
    firstInBounds := centerInBounds
    firstFloor := setFloor_at parameters.dimensions.width carved center
      (by
        apply positionIndex_lt parameters carved
        · simp [carved, base, carveRoom_size, MapGenerationParameters.mapArea]
        · exact centerInBounds)
    previous := room }

private def generateState (parameters : MapGenerationParameters) : IO (GenerationState parameters) := do
  let first ← randomRoom parameters
  let mut state := initialState parameters first
  for _ in [1:parameters.attempts] do
    let room ← randomRoom parameters
    let turnRoll ← Random.fin 2 (by omega)
    state := tryRoom parameters state room.1 (turnRoll == 0)
  return state

public def generateMap (parameters : MapGenerationParameters) : IO GeneratedMap := do
  let state ← generateState parameters
  let map := Map.ofTiles parameters.dimensions state.tiles state.tilesSize
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map state.first (by
    simp [map, Map.tileAt?_ofTiles, state.firstInBounds, state.firstFloor])

end Gimlight

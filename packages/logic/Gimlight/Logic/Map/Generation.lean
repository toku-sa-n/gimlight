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

private structure SizedTiles (parameters : MapGenerationParameters) where
  tiles : Array Tile
  sizeEq : tiles.size = parameters.mapArea

private def SizedTiles.walls (parameters : MapGenerationParameters) : SizedTiles parameters :=
  { tiles := Array.replicate parameters.mapArea .wall
    sizeEq := by simp }

private def SizedTiles.setFloor (tiles : SizedTiles parameters) (x y : Nat) :
    SizedTiles parameters :=
  { tiles := tiles.tiles.set! (y * parameters.dimensions.width + x) .floor
    sizeEq := by simpa [Array.size_setIfInBounds] using tiles.sizeEq }

private def SizedTiles.carveRoom (tiles : SizedTiles parameters) (room : Room) :
    SizedTiles parameters :=
  (List.range' room.y room.height).foldl (fun result y =>
    (List.range' room.x room.width).foldl (fun result x => result.setFloor x y) result)
    tiles

private def SizedTiles.carveHorizontal (tiles : SizedTiles parameters)
    (y sourceX targetX : Nat) : SizedTiles parameters :=
  let low := min sourceX targetX
  let high := max sourceX targetX
  (List.range' low (high + 1 - low)).foldl (fun result x => result.setFloor x y) tiles

private def SizedTiles.carveVertical (tiles : SizedTiles parameters)
    (x sourceY targetY : Nat) : SizedTiles parameters :=
  let low := min sourceY targetY
  let high := max sourceY targetY
  (List.range' low (high + 1 - low)).foldl (fun result y => result.setFloor x y) tiles

private def SizedTiles.carveTunnel (tiles : SizedTiles parameters) (source target : Position)
    (horizontalFirst : Bool) : SizedTiles parameters :=
  if horizontalFirst then
    (tiles.carveHorizontal source.y source.x target.x).carveVertical target.x source.y target.y
  else
    (tiles.carveVertical source.x source.y target.y).carveHorizontal target.y source.x target.x

private structure GenerationState (parameters : MapGenerationParameters) where
  tiles : SizedTiles parameters
  rooms : Array Room
  first : Position
  firstInBounds : first.x < parameters.dimensions.width ∧
    first.y < parameters.dimensions.height
  firstFloor : tiles.tiles[first.y * parameters.dimensions.width + first.x]? = some .floor
  previous : Room

private def CandidateRoomValid (parameters : MapGenerationParameters) (room : Room) : Prop :=
  parameters.minRoomDimensions.width ≤ room.width ∧
    room.width ≤ parameters.maxRoomDimensions.width ∧
    parameters.minRoomDimensions.height ≤ room.height ∧
    room.height ≤ parameters.maxRoomDimensions.height ∧
    parameters.outerWallMargin ≤ room.x ∧
    room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
    parameters.outerWallMargin ≤ room.y ∧
    room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height

private abbrev CandidateRoom (parameters : MapGenerationParameters) :=
  { room : Room // CandidateRoomValid parameters room }

private def candidateRoom (parameters : MapGenerationParameters)
    (widthRoll : Fin parameters.roomWidthRange)
    (heightRoll : Fin parameters.roomHeightRange)
    (xRoll : Fin parameters.roomXRange)
    (yRoll : Fin parameters.roomYRange) : CandidateRoom parameters :=
  let roomWidth := parameters.minRoomDimensions.width + widthRoll.val
  let roomHeight := parameters.minRoomDimensions.height + heightRoll.val
  let room : Room :=
    { x := parameters.outerWallMargin + xRoll.val
      y := parameters.outerWallMargin + yRoll.val
      width := roomWidth
      height := roomHeight }
  ⟨room, by
  obtain ⟨_, _, _, _, widthFits, heightFits, _⟩ := parameters.valid
  have widthRollInRange := widthRoll.isLt
  have heightRollInRange := heightRoll.isLt
  have xRollInRange := xRoll.isLt
  have yRollInRange := yRoll.isLt
  simp [CandidateRoomValid, room, roomWidth, roomHeight,
    MapGenerationParameters.roomWidthRange, MapGenerationParameters.roomHeightRange,
    MapGenerationParameters.roomXRange, MapGenerationParameters.roomYRange] at *
  omega⟩

private theorem SizedTiles.positionIndex_lt (tiles : SizedTiles parameters) (position : Position)
    (inBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height) :
    position.y * parameters.dimensions.width + position.x < tiles.tiles.size := by
  rw [tiles.sizeEq]
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

private theorem SizedTiles.setFloor_at (tiles : SizedTiles parameters) (position : Position)
    (inBounds : position.y * parameters.dimensions.width + position.x < tiles.tiles.size) :
    (tiles.setFloor position.x position.y).tiles[
      position.y * parameters.dimensions.width + position.x]? =
      some .floor := by
  simp [SizedTiles.setFloor, inBounds]

private def tryRoom (parameters : MapGenerationParameters) (state : GenerationState parameters)
    (room : Room) (horizontalFirst : Bool) : GenerationState parameters :=
  if state.rooms.any (!room.separated ·) then
    state
  else
    let center := room.center
    let withRoom := state.tiles.carveRoom room
    let withTunnel := withRoom.carveTunnel state.previous.center center horizontalFirst
    -- Reapply the start floor because carving tracks size, but deliberately carries no tile proofs.
    let tiles := withTunnel.setFloor state.first.x state.first.y
    { tiles
      rooms := state.rooms.push room
      first := state.first
      firstInBounds := state.firstInBounds
      firstFloor := withTunnel.setFloor_at state.first
        (withTunnel.positionIndex_lt state.first state.firstInBounds)
      previous := room }

private def randomRoom (parameters : MapGenerationParameters) : IO (CandidateRoom parameters) := do
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
  return candidateRoom parameters widthRoll heightRoll xRoll yRoll

private def initialState (parameters : MapGenerationParameters) (candidate : CandidateRoom parameters) :
    GenerationState parameters :=
  let room := candidate.1
  let center : Position := { x := room.x + room.width / 2, y := room.y + room.height / 2 }
  let carved := (SizedTiles.walls parameters).carveRoom room
  let tiles := carved.setFloor center.x center.y
  have centerInBounds : center.x < parameters.dimensions.width ∧
      center.y < parameters.dimensions.height := by
    have inside := candidate.2
    have widthHalf := Nat.div_le_self room.width 2
    have heightHalf := Nat.div_le_self room.height 2
    change room.x + room.width / 2 < parameters.dimensions.width ∧
      room.y + room.height / 2 < parameters.dimensions.height
    change CandidateRoomValid parameters room at inside
    simp only [CandidateRoomValid] at inside
    omega
  { tiles
    rooms := #[room]
    first := center
    firstInBounds := centerInBounds
    firstFloor := carved.setFloor_at center (carved.positionIndex_lt center centerInBounds)
    previous := room }

private def generateState (parameters : MapGenerationParameters) : IO (GenerationState parameters) := do
  let first ← randomRoom parameters
  let mut state := initialState parameters first
  for _ in [1:parameters.attempts] do
    let room ← randomRoom parameters
    let horizontalFirst ← Random.bool
    state := tryRoom parameters state room.1 horizontalFirst
  return state

public def generateMap (parameters : MapGenerationParameters) : IO GeneratedMap := do
  let state ← generateState parameters
  let map := Map.ofTiles parameters.dimensions state.tiles.tiles state.tiles.sizeEq
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map state.first (by
    simp [map, Map.tileAt?_ofTiles, state.firstInBounds, state.firstFloor])

end Gimlight

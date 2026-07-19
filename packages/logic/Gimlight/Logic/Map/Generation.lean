module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
import Gimlight.Logic.Map.Generation.Tiles
import Gimlight.Logic.Map.Generation.CandidateRoom
import Gimlight.Logic.Random
public meta import Gimlight.Logic.Map.Core
public meta import Gimlight.Logic.Room

namespace Gimlight

public structure GeneratedMap where private mk ::
  public map : Map
  public start : Position
  public startOnFloor : map.tileAt? start = some .floor

namespace MapGeneration

private structure GenerationState (parameters : MapGenerationParameters) where
  tiles : SizedTiles parameters
  rooms : Array Room
  first : Position
  firstInBounds : first.x < parameters.dimensions.width ∧
    first.y < parameters.dimensions.height
  firstFloor : tiles.tiles[first.y * parameters.dimensions.width + first.x]? = some .floor
  previous : Room

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

end MapGeneration

public def generateMap (parameters : MapGenerationParameters) : IO GeneratedMap := do
  let state ← MapGeneration.generateState parameters
  let map := Map.ofTiles parameters.dimensions state.tiles.tiles state.tiles.sizeEq
    (fun source target fromFloor toFloor => ⟨fromFloor, toFloor⟩)
  return .mk map state.first (by
    simp [map, Map.tileAt?_ofTiles, state.firstInBounds, state.firstFloor])

end Gimlight

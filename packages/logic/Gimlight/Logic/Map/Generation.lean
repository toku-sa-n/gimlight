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
  start : Position
  startInBounds : start.x < parameters.dimensions.width ∧
    start.y < parameters.dimensions.height
  startFloor : tiles.tiles[start.y * parameters.dimensions.width + start.x]? = some .floor
  lastRoom : Room

private def GenerationState.withTiles (state : GenerationState parameters)
    (updated : SizedTiles parameters) : GenerationState parameters :=
  let tiles := updated.setFloorAt state.start
  { state with
    tiles
    startFloor := updated.setFloorAt_at state.start state.startInBounds }

private def GenerationState.canPlace (state : GenerationState parameters) (room : Room) : Bool :=
  !state.rooms.any (!room.separated ·)

private def GenerationState.carve (state : GenerationState parameters) (room : Room)
    (horizontalFirst : Bool) : GenerationState parameters :=
  let tiles := (state.tiles.carveRoom room).carveTunnel state.lastRoom.center room.center
    horizontalFirst
  { state.withTiles tiles with
    rooms := state.rooms.push room
    lastRoom := room }

private def tryRoom (parameters : MapGenerationParameters) (state : GenerationState parameters)
    (room : Room) (horizontalFirst : Bool) : GenerationState parameters :=
  if state.canPlace room then state.carve room horizontalFirst else state

private def initialState (parameters : MapGenerationParameters) (candidate : CandidateRoom parameters) :
    GenerationState parameters :=
  let room := candidate.1
  let start := candidate.center
  let carved := (SizedTiles.walls parameters).carveRoom room
  let tiles := carved.setFloorAt start
  { tiles
    rooms := #[room]
    start
    startInBounds := candidate.centerInBounds
    startFloor := carved.setFloorAt_at start candidate.centerInBounds
    lastRoom := room }

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
  return .mk map state.start (by
    simp [map, Map.tileAt?_ofTiles, state.startInBounds, state.startFloor])

end Gimlight

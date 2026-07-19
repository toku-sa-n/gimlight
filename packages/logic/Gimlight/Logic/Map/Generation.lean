module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
import Gimlight.Logic.Map.Generation.Connectivity
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
  rooms : Array Room
  lastCenter : Position
  tiles : RootedConnectedTiles parameters lastCenter

private def GenerationState.canPlace (state : GenerationState parameters) (room : Room) : Bool :=
  !state.rooms.any (!room.separated ·)

private def GenerationState.carve (state : GenerationState parameters)
    (candidate : CandidateRoom parameters)
    (horizontalFirst : Bool) : GenerationState parameters :=
  let tiles := state.tiles.addRoom candidate horizontalFirst
  { rooms := state.rooms.push candidate.1
    lastCenter := candidate.center
    tiles }

private def tryRoom (parameters : MapGenerationParameters) (state : GenerationState parameters)
    (candidate : CandidateRoom parameters) (horizontalFirst : Bool) : GenerationState parameters :=
  if state.canPlace candidate.1 then state.carve candidate horizontalFirst else state

private def initialState (parameters : MapGenerationParameters) (candidate : CandidateRoom parameters) :
    GenerationState parameters :=
  { rooms := #[candidate.1]
    lastCenter := candidate.center
    tiles := ConnectedTiles.initialRoom candidate }

private def generateState (parameters : MapGenerationParameters) : IO (GenerationState parameters) := do
  let first ← randomRoom parameters
  let mut state := initialState parameters first
  for _ in [1:parameters.attempts] do
    let room ← randomRoom parameters
    let horizontalFirst ← Random.bool
    state := tryRoom parameters state room horizontalFirst
  return state

end MapGeneration

public def generateMap (parameters : MapGenerationParameters) : IO GeneratedMap := do
  let state ← MapGeneration.generateState parameters
  let connected := state.tiles.connected
  let map := Map.ofTiles parameters.dimensions connected.tiles.tiles connected.tiles.sizeEq
    connected.allFloorsConnected
  return .mk map connected.start (by
    simp [map, Map.tileAt?_ofTiles, connected.startInBounds, connected.startFloor])

end Gimlight

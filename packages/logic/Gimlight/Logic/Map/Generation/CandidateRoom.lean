module

public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
import Gimlight.Logic.Random
public meta import Gimlight.Logic.Room

namespace Gimlight.MapGeneration

public abbrev CandidateRoomValid (parameters : MapGenerationParameters) (room : Room) : Prop :=
  parameters.minRoomDimensions.width ≤ room.width ∧
    room.width ≤ parameters.maxRoomDimensions.width ∧
    parameters.minRoomDimensions.height ≤ room.height ∧
    room.height ≤ parameters.maxRoomDimensions.height ∧
    parameters.outerWallMargin ≤ room.x ∧
    room.x + room.width + parameters.outerWallMargin < parameters.dimensions.width ∧
    parameters.outerWallMargin ≤ room.y ∧
    room.y + room.height + parameters.outerWallMargin < parameters.dimensions.height

public abbrev CandidateRoom (parameters : MapGenerationParameters) :=
  { room : Room // CandidateRoomValid parameters room }

public def randomRoom (parameters : MapGenerationParameters) : IO (CandidateRoom parameters) := do
  let ⟨_, widthOrdered, _, heightOrdered, widthFits, heightFits, _⟩ := parameters.valid
  let roomWidth ← Random.natRange parameters.minRoomDimensions.width
    (parameters.maxRoomDimensions.width + 1) (by omega)
  let roomHeight ← Random.natRange parameters.minRoomDimensions.height
    (parameters.maxRoomDimensions.height + 1) (by omega)
  let roomX ← Random.natRange parameters.outerWallMargin
    (parameters.dimensions.width - parameters.maxRoomDimensions.width -
      parameters.outerWallMargin) (by omega)
  let roomY ← Random.natRange parameters.outerWallMargin
    (parameters.dimensions.height - parameters.maxRoomDimensions.height -
      parameters.outerWallMargin) (by omega)
  let ⟨roomWidthMin, roomWidthMax⟩ := roomWidth.property
  let ⟨roomHeightMin, roomHeightMax⟩ := roomHeight.property
  let ⟨roomXMin, roomXMax⟩ := roomX.property
  let ⟨roomYMin, roomYMax⟩ := roomY.property
  let room : Room :=
    { x := roomX.val
      y := roomY.val
      width := roomWidth.val
      height := roomHeight.val }
  return ⟨room, by
    simp only [CandidateRoomValid, room]
    omega⟩

end Gimlight.MapGeneration

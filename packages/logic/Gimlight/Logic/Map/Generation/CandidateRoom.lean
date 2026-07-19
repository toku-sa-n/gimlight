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

private def candidateRoom (parameters : MapGenerationParameters)
    (roomWidth : NatRange parameters.minRoomDimensions.width
      (parameters.maxRoomDimensions.width + 1))
    (roomHeight : NatRange parameters.minRoomDimensions.height
      (parameters.maxRoomDimensions.height + 1))
    (roomX : NatRange parameters.outerWallMargin
      (parameters.dimensions.width - parameters.maxRoomDimensions.width -
        parameters.outerWallMargin))
    (roomY : NatRange parameters.outerWallMargin
      (parameters.dimensions.height - parameters.maxRoomDimensions.height -
        parameters.outerWallMargin)) : CandidateRoom parameters :=
  let room : Room :=
    { x := roomX.val
      y := roomY.val
      width := roomWidth.val
      height := roomHeight.val }
  ⟨room, by
  obtain ⟨_, _, _, _, widthFits, heightFits, _⟩ := parameters.valid
  obtain ⟨widthMin, widthMax⟩ := roomWidth.property
  obtain ⟨heightMin, heightMax⟩ := roomHeight.property
  obtain ⟨xMin, xMax⟩ := roomX.property
  obtain ⟨yMin, yMax⟩ := roomY.property
  simp [CandidateRoomValid, room] at *
  omega⟩

public def randomRoom (parameters : MapGenerationParameters) : IO (CandidateRoom parameters) := do
  have widthOrdered := parameters.valid.2.1
  have heightOrdered := parameters.valid.2.2.2.1
  have widthFits := parameters.valid.2.2.2.2.1
  have heightFits := parameters.valid.2.2.2.2.2.1
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
  return candidateRoom parameters roomWidth roomHeight roomX roomY

end Gimlight.MapGeneration

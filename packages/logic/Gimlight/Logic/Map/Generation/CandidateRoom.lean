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

public def CandidateRoom.center (candidate : CandidateRoom parameters) : Position :=
  { x := candidate.1.x + candidate.1.width / 2
    y := candidate.1.y + candidate.1.height / 2 }

public theorem CandidateRoom.centerInBounds (candidate : CandidateRoom parameters) :
    candidate.center.x < parameters.dimensions.width ∧
      candidate.center.y < parameters.dimensions.height := by
  have inside := candidate.2
  have widthHalf := Nat.div_le_self candidate.1.width 2
  have heightHalf := Nat.div_le_self candidate.1.height 2
  change CandidateRoomValid parameters candidate.1 at inside
  simp only [CandidateRoomValid] at inside
  simp only [CandidateRoom.center]
  omega

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

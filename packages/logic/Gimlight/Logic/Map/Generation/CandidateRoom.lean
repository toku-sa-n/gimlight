module

public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
import Gimlight.Logic.Random
public meta import Gimlight.Logic.Room

namespace Gimlight.MapGeneration

private def roomWidthRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomDimensions.width - parameters.minRoomDimensions.width + 1

private def roomHeightRange (parameters : MapGenerationParameters) : Nat :=
  parameters.maxRoomDimensions.height - parameters.minRoomDimensions.height + 1

private def roomXRange (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.width - (parameters.maxRoomDimensions.width + parameters.outerWallMargin * 2)

private def roomYRange (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.height - (parameters.maxRoomDimensions.height + parameters.outerWallMargin * 2)

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
    (widthRoll : Fin (roomWidthRange parameters))
    (heightRoll : Fin (roomHeightRange parameters))
    (xRoll : Fin (roomXRange parameters))
    (yRoll : Fin (roomYRange parameters)) : CandidateRoom parameters :=
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
    roomWidthRange, roomHeightRange, roomXRange, roomYRange] at *
  omega⟩

public def randomRoom (parameters : MapGenerationParameters) : IO (CandidateRoom parameters) := do
  let widthRoll ← Random.fin (roomWidthRange parameters) (by
    simp [roomWidthRange])
  let heightRoll ← Random.fin (roomHeightRange parameters) (by
    simp [roomHeightRange])
  let xRoll ← Random.fin (roomXRange parameters) (by
    simp [roomXRange]
    obtain ⟨_, _, _, _, widthFits, _, _⟩ := parameters.valid
    omega)
  let yRoll ← Random.fin (roomYRange parameters) (by
    simp [roomYRange]
    obtain ⟨_, _, _, _, _, heightFits, _⟩ := parameters.valid
    omega)
  return candidateRoom parameters widthRoll heightRoll xRoll yRoll

end Gimlight.MapGeneration

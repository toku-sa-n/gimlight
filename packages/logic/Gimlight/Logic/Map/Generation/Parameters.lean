module

public import Gimlight.Logic.Dimensions

namespace Gimlight

public structure MapGenerationParameters where private mk ::
  public dimensions : Dimensions
  public minRoomDimensions : Dimensions
  public maxRoomDimensions : Dimensions
  public outerWallMargin : Nat
  public attempts : Nat
  private minRoomWidthPositive : 0 < minRoomDimensions.width
  private roomWidthOrdered : minRoomDimensions.width ≤ maxRoomDimensions.width
  private minRoomHeightPositive : 0 < minRoomDimensions.height
  private roomHeightOrdered : minRoomDimensions.height ≤ maxRoomDimensions.height
  private roomWidthFits : maxRoomDimensions.width + outerWallMargin * 2 < dimensions.width
  private roomHeightFits : maxRoomDimensions.height + outerWallMargin * 2 < dimensions.height
  private attemptsPositive : 0 < attempts

public theorem MapGenerationParameters.valid (parameters : MapGenerationParameters) :
    0 < parameters.minRoomDimensions.width ∧
      parameters.minRoomDimensions.width ≤ parameters.maxRoomDimensions.width ∧
      0 < parameters.minRoomDimensions.height ∧
      parameters.minRoomDimensions.height ≤ parameters.maxRoomDimensions.height ∧
      parameters.maxRoomDimensions.width + parameters.outerWallMargin * 2 <
        parameters.dimensions.width ∧
      parameters.maxRoomDimensions.height + parameters.outerWallMargin * 2 <
        parameters.dimensions.height ∧
      0 < parameters.attempts :=
  ⟨parameters.minRoomWidthPositive, parameters.roomWidthOrdered,
    parameters.minRoomHeightPositive, parameters.roomHeightOrdered, parameters.roomWidthFits,
    parameters.roomHeightFits, parameters.attemptsPositive⟩

public def MapGenerationParameters.create? (dimensions minRoomDimensions maxRoomDimensions : Dimensions)
    (outerWallMargin attempts : Nat) : Option MapGenerationParameters :=
  if valid : 0 < minRoomDimensions.width ∧
      minRoomDimensions.width ≤ maxRoomDimensions.width ∧
      0 < minRoomDimensions.height ∧
      minRoomDimensions.height ≤ maxRoomDimensions.height ∧
      maxRoomDimensions.width + outerWallMargin * 2 < dimensions.width ∧
      maxRoomDimensions.height + outerWallMargin * 2 < dimensions.height ∧
      0 < attempts then
    let ⟨minRoomWidthPositive, roomWidthOrdered, minRoomHeightPositive, roomHeightOrdered,
      roomWidthFits, roomHeightFits, attemptsPositive⟩ := valid
    some (.mk dimensions minRoomDimensions maxRoomDimensions outerWallMargin attempts
      minRoomWidthPositive roomWidthOrdered minRoomHeightPositive roomHeightOrdered roomWidthFits
      roomHeightFits attemptsPositive)
  else none

public def MapGenerationParameters.default : MapGenerationParameters :=
  .mk { width := 60, height := 30 } { width := 5, height := 4 } { width := 12, height := 8 }
    1 30 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

end Gimlight

module

public import Gimlight.Logic.Map.Core
public import Gimlight.Logic.Room
public import Gimlight.Logic.Map.Generation.Parameters
public meta import Gimlight.Logic.Map.Core

namespace Gimlight.MapGeneration

public abbrev mapArea (parameters : MapGenerationParameters) : Nat :=
  parameters.dimensions.width * parameters.dimensions.height

public structure SizedTiles (parameters : MapGenerationParameters) where
  public tiles : Array Tile
  public sizeEq : tiles.size = mapArea parameters

public def SizedTiles.walls (parameters : MapGenerationParameters) : SizedTiles parameters :=
  { tiles := Array.replicate (mapArea parameters) .wall
    sizeEq := by simp }

public theorem SizedTiles.walls_not_floor (parameters : MapGenerationParameters)
    (position : Position) :
    (SizedTiles.walls parameters).tiles[
      position.y * parameters.dimensions.width + position.x]? ≠ some .floor := by
  simp only [SizedTiles.walls, Array.getElem?_replicate]
  split <;> simp

public def SizedTiles.setFloor (tiles : SizedTiles parameters) (x y : Nat) :
    SizedTiles parameters :=
  { tiles := tiles.tiles.set! (y * parameters.dimensions.width + x) .floor
    sizeEq := by simpa [Array.size_setIfInBounds] using tiles.sizeEq }

public def SizedTiles.setFloorAt (tiles : SizedTiles parameters) (position : Position) :
    SizedTiles parameters :=
  tiles.setFloor position.x position.y

public def SizedTiles.carveRoom (tiles : SizedTiles parameters) (room : Room) :
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

public def SizedTiles.carveTunnel (tiles : SizedTiles parameters) (source target : Position)
    (horizontalFirst : Bool) : SizedTiles parameters :=
  if horizontalFirst then
    (tiles.carveHorizontal source.y source.x target.x).carveVertical target.x source.y target.y
  else
    (tiles.carveVertical source.x source.y target.y).carveHorizontal target.y source.x target.x

private theorem SizedTiles.positionIndex_lt (tiles : SizedTiles parameters) (position : Position)
    (inBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height) :
    position.y * parameters.dimensions.width + position.x < tiles.tiles.size := by
  rw [tiles.sizeEq]
  simp [mapArea]
  calc
    position.y * parameters.dimensions.width + position.x <
        (position.y + 1) * parameters.dimensions.width := by
      rw [Nat.add_mul]
      simpa using Nat.add_lt_add_left inBounds.1
        (position.y * parameters.dimensions.width)
    _ ≤ parameters.dimensions.height * parameters.dimensions.width :=
      Nat.mul_le_mul_right parameters.dimensions.width inBounds.2
    _ = parameters.dimensions.width * parameters.dimensions.height := Nat.mul_comm _ _

public theorem SizedTiles.setFloorAt_at (tiles : SizedTiles parameters) (position : Position)
    (inBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height) :
    (tiles.setFloorAt position).tiles[
      position.y * parameters.dimensions.width + position.x]? =
      some .floor := by
  simp [SizedTiles.setFloorAt, SizedTiles.setFloor, tiles.positionIndex_lt position inBounds]

public theorem SizedTiles.setFloorAt_preservesFloor (tiles : SizedTiles parameters)
    (position target : Position)
    (positionInBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height)
    (targetFloor : tiles.tiles[target.y * parameters.dimensions.width + target.x]? =
      some .floor) :
    (tiles.setFloorAt position).tiles[
      target.y * parameters.dimensions.width + target.x]? = some .floor := by
  simp [SizedTiles.setFloorAt, SizedTiles.setFloor, Array.getElem?_setIfInBounds,
    tiles.positionIndex_lt position positionInBounds, targetFloor]

public theorem SizedTiles.floor_of_setFloorAt_floor_of_index_ne
    (tiles : SizedTiles parameters) (position target : Position)
    (indexNe : position.y * parameters.dimensions.width + position.x ≠
      target.y * parameters.dimensions.width + target.x)
    (targetFloor : (tiles.setFloorAt position).tiles[
      target.y * parameters.dimensions.width + target.x]? = some .floor) :
    tiles.tiles[target.y * parameters.dimensions.width + target.x]? = some .floor := by
  simpa [SizedTiles.setFloorAt, SizedTiles.setFloor, Array.getElem?_setIfInBounds, indexNe]
    using targetFloor

end Gimlight.MapGeneration

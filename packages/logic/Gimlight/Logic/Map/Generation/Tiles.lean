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

public def SizedTiles.setFloor (tiles : SizedTiles parameters) (x y : Nat) :
    SizedTiles parameters :=
  { tiles := tiles.tiles.set! (y * parameters.dimensions.width + x) .floor
    sizeEq := by simpa [Array.size_setIfInBounds] using tiles.sizeEq }

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

public theorem SizedTiles.positionIndex_lt (tiles : SizedTiles parameters) (position : Position)
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

public theorem SizedTiles.setFloor_at (tiles : SizedTiles parameters) (position : Position)
    (inBounds : position.y * parameters.dimensions.width + position.x < tiles.tiles.size) :
    (tiles.setFloor position.x position.y).tiles[
      position.y * parameters.dimensions.width + position.x]? =
      some .floor := by
  simp [SizedTiles.setFloor, inBounds]

end Gimlight.MapGeneration

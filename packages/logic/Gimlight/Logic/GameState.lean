module

public import Gimlight.Logic.Map
public import Gimlight.Logic.Position
public import Gimlight.Logic.Direction

namespace Gimlight

public structure GameState where private mk ::
  public map : Map
  public player : Position
  private playerOnFloor : map.tileAt? player = some .floor
deriving Repr

public def initialState : IO GameState := do
  let generated ← generateMap
  return .mk generated.map generated.start generated.start_is_floor

public def move (direction : Direction) (state : GameState) : GameState :=
  match state.player.step direction with
  | some target =>
    if targetFloor : state.map.tileAt? target = some .floor then
      .mk state.map target targetFloor
    else
      state
  | none => state

public theorem move_into_wall_unchanged (direction : Direction) (state : GameState)
    (wall : (state.player.step direction).all fun target => state.map.tileAt? target = some .wall) :
    move direction state = state := by
  simp [move]
  split
  · rename_i target step
    simp [step] at wall
    simp [wall]
  · rfl

public theorem move_out_of_bounds_unchanged (direction : Direction) (state : GameState)
    (outOfBounds : (state.player.step direction).all fun target => state.map.tileAt? target = none) :
    move direction state = state := by
  simp [move]
  split
  · rename_i target step
    simp [step] at outOfBounds
    simp [outOfBounds]
  · rfl

end Gimlight

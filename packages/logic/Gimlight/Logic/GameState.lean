module

public import Gimlight.Logic.Map
public import Gimlight.Logic.Position
public import Gimlight.Logic.Direction

namespace Gimlight

public structure GameState where private mk ::
  public map : Map
  public player : Position
  private playerOnFloor : map.tileAt player = .floor
deriving Repr

public def initialState : IO GameState := do
  let map ← generateMap
  return .mk map map.start map.start_is_floor

public def move (direction : Direction) (state : GameState) : GameState :=
  match state.player.step direction with
  | some target =>
    if targetFloor : state.map.tileAt target = .floor then
      .mk state.map target targetFloor
    else
      state
  | none => state

public theorem GameState.player_is_floor (state : GameState) :
    state.map.tileAt state.player = .floor := state.playerOnFloor

public theorem move_into_wall_unchanged (direction : Direction) (state : GameState)
    (wall : (state.player.step direction).all fun target => state.map.tileAt target = .wall) :
    move direction state = state := by
  simp [move]
  split
  · rename_i target step
    simp [step] at wall
    simp [wall]
  · rfl

public theorem move_preserves_floor (direction : Direction) (state : GameState) :
    (move direction state).map.tileAt (move direction state).player = .floor :=
  (move direction state).playerOnFloor

public theorem moves_preserve_floor (directions : List Direction) (state : GameState) :
    let final := directions.foldl (fun current direction => move direction current) state
    final.map.tileAt final.player = .floor := by
  induction directions generalizing state with
  | nil => exact state.playerOnFloor
  | cons direction rest ih => simpa using ih (move direction state)

end Gimlight

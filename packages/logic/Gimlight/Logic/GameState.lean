module

public import Gimlight.Logic.Map
public import Gimlight.Logic.Position
public import Gimlight.Logic.Direction

namespace Gimlight

public structure GameState where private mk ::
  public map : Map
  public player : Position
  private playerInBounds : player.inBounds map
deriving DecidableEq, Repr

public def initialState : GameState :=
  .mk defaultMap (.centeredOn defaultMap) (Position.centeredOn_in_bounds defaultMap)

public def move (direction : Direction) (state : GameState) : GameState :=
  .mk state.map (state.player.move state.map direction)
    (Position.move_preserves_bounds state.map direction state.player state.playerInBounds)

end Gimlight

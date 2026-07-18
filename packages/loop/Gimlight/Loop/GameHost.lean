module

public import Gimlight.Loop.GameInput
public import Gimlight.Loop.GameView
public import Gimlight.Logic.GameState

namespace Gimlight

public class GameHost (m : Type -> Type) where
  render : GameView -> m Unit
  readInput : m GameInput

namespace GameHost

private def gameViewOfState (state : GameState) : GameView :=
  { width := state.map.dimensions.width
    height := state.map.dimensions.height
    tiles := Array.ofFn fun index : Fin (state.map.dimensions.width * state.map.dimensions.height) =>
      let position : Position :=
        { x := index.val % state.map.dimensions.width, y := index.val / state.map.dimensions.width }
      match state.map.tileAt position with
      | .wall => .wall
      | .floor => .floor
    playerX := state.player.x
    playerY := state.player.y }

private partial def gameLoopFrom {m : Type -> Type} [Monad m] [GameHost m]
    (state : GameState) : m Unit := do
  render (gameViewOfState state)
  match ← readInput (m := m) with
  | .quit => pure ()
  | .move .left => gameLoopFrom (move .left state)
  | .move .right => gameLoopFrom (move .right state)
  | .move .up => gameLoopFrom (move .up state)
  | .move .down => gameLoopFrom (move .down state)

-- Keep specialization from leaking the private loop implementation into callers.
-- The aggregate `Gimlight.Loop` module imports this module privately, so its
-- public API still does not re-export `Gimlight.Logic`.
@[nospecialize]
public def gameLoop {m : Type -> Type} [Monad m] [GameHost m] (initial : GameState) : m Unit :=
  gameLoopFrom initial

end GameHost

end Gimlight

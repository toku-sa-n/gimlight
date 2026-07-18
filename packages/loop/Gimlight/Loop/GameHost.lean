module

public import Gimlight.Loop.GameInput
public import Gimlight.Loop.GameView
import Gimlight.Logic.GameState

namespace Gimlight

public class GameHost (m : Type -> Type) where
  render : GameView -> m Unit
  readInput : m GameInput

namespace GameHost

private def gameViewOfState (state : GameState) : GameView :=
  { width := state.map.dimensions.width
    height := state.map.dimensions.height
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

-- Without this annotation, this definition only compiles if `Gimlight.Logic`
-- is imported publicly. That is not appropriate here: more concrete packages
-- should not be able to depend on the game logic through `Gimlight.Loop`.
@[nospecialize]
public def gameLoop {m : Type -> Type} [Monad m] [GameHost m] : m Unit :=
  gameLoopFrom initialState

end GameHost

end Gimlight

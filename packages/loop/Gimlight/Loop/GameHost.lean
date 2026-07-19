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
      have widthPositive : 0 < state.map.dimensions.width := by
        apply Nat.pos_of_ne_zero
        intro widthZero
        simp [widthZero] at index
        exact Fin.elim0 index
      let position : Position :=
        { x := index.val % state.map.dimensions.width, y := index.val / state.map.dimensions.width }
      have positionInBounds :
          position.x < state.map.dimensions.width ∧ position.y < state.map.dimensions.height := by
        constructor
        · exact Nat.mod_lt _ widthPositive
        · apply (Nat.div_lt_iff_lt_mul widthPositive).2
          exact Nat.lt_of_lt_of_eq index.isLt (Nat.mul_comm _ _)
      match tile : state.map.tileAt? position with
      | some .wall => .wall
      | some .floor => .floor
      | none => by
        exact (state.map.tileAt?_ne_none_of_in_bounds position positionInBounds tile).elim
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

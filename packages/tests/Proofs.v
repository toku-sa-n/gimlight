From Gimlight.Logic Require Import Logic.

Print Assumptions Position.centeredOn_in_bounds.
Print Assumptions Position.move_preserves_bounds.
Print Assumptions GameState.playerInBounds.

Theorem initial_player_is_in_bounds :
  Position.in_bounds (GameState.player GameState.initialState).
Proof.
  apply GameState.playerInBounds.
Qed.

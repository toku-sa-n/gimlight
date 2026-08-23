From Gimlight.Logic Require Import Logic.

Check Dimensions.t.
Check Dimensions.width.
Check Dimensions.height.
Check Direction.t.
Check Map.dimensions.
Check Map.make.
Check Position.t.
Check Position.x.
Check Position.x_value.
Check Position.centeredOn.
Check Position.position_in_bounds.
Check GameState.player.
Check GameState.playerInBounds.

Fail Check Position.coordinate.
Fail Check Position.coordinate_in_bounds.
Fail Check Position.coordinate_left.
Fail Check Position.coordinate_right.
Fail Check Position.coordinate_up.
Fail Check Position.coordinate_down.
Fail Check Position.centered_coordinate.

Print Assumptions Position.centeredOn_in_bounds.
Print Assumptions Position.move_preserves_bounds.
Print Assumptions GameState.playerInBounds.

Theorem initial_player_is_in_bounds :
  Position.in_bounds (GameState.player GameState.initialState).
Proof.
  apply GameState.playerInBounds.
Qed.

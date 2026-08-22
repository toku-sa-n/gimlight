Require Import Map Position Direction.

Module GameState.

Record t : Set := {
  map : Map.t;
  player : Position.t map
}.

Definition initialState : t :=
  {| map := Map.default;
     player := Position.centeredOn Map.default |}.

Definition move (direction : Direction.t) (state : t) : t :=
  {| map := map state;
     player := Position.move (map state) direction (player state) |}.

Theorem playerInBounds (state : t) :
  Position.in_bounds (player state).
Proof.
  apply Position.position_in_bounds.
Qed.

End GameState.

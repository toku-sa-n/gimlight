Require Import Map Position Direction.

Module Type GameStateApi.

Record t : Set := {
  map : Map.t;
  player : Position.t map
}.

Parameter initialState : t.

Parameter move : Direction.t -> t -> t.

Axiom playerInBounds : forall (state : t),
  Position.in_bounds (player state).

End GameStateApi.

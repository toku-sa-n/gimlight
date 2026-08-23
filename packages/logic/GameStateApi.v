Require Import Map Position Direction.

Module Type GameStateApi.

Parameter t : Set.

Parameter map : t -> Map.t.

Parameter player : forall (state : t), Position.t (map state).

Parameter initialState : t.

Parameter move : Direction.t -> t -> t.

Axiom playerInBounds : forall (state : t),
  Position.in_bounds (player state).

End GameStateApi.

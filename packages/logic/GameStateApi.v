Require Import DimensionsApi MapApi DirectionApi PositionApi.

Module Type GameStateApi
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi)
    (Position : PositionApi Dimensions Map Direction).

Parameter t : Set.

Parameter map : t -> Map.t.

Parameter player : forall (state : t), Position.t (map state).

Parameter initialState : t.

Parameter move : Direction.t -> t -> t.

Axiom playerInBounds : forall (state : t),
  Position.in_bounds (player state).

End GameStateApi.

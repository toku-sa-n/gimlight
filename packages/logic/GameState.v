Require Import Dimensions Map Direction Position
  DimensionsApi MapApi DirectionApi PositionApi GameStateApi.

Module MakeGameState
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi)
    (Position : PositionApi Dimensions Map Direction) :
    GameStateApi Dimensions Map Direction Position.

Record state : Set := {
  map : Map.t;
  player : Position.t map
}.

Definition t : Set := state.

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

End MakeGameState.

Module GameState : GameStateApi Dimensions Map Direction Position :=
  MakeGameState Dimensions Map Direction Position.

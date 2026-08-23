Require Import DimensionsApi MapApi DirectionApi PositionApi
  GameInputApi GameStateApi.

Module Type GameStepApi
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi)
    (Position : PositionApi Dimensions Map Direction)
    (GameInput : GameInputApi Direction)
    (GameState : GameStateApi Dimensions Map Direction Position).

Inductive result : Set :=
| Continue (state : GameState.t)
| Quit.

Parameter step : GameInput.t -> GameState.t -> result.

End GameStepApi.

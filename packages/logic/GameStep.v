Require Import Dimensions Map Direction Position GameInput GameState
  DimensionsApi MapApi DirectionApi PositionApi GameInputApi GameStateApi
  GameStepApi.

Module Make
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi)
    (Position : PositionApi Dimensions Map Direction)
    (GameInput : GameInputApi Direction)
    (GameState : GameStateApi Dimensions Map Direction Position) :
    GameStepApi Dimensions Map Direction Position GameInput GameState.

Inductive result : Set :=
| Continue (state : GameState.t)
| Quit.

Definition step (input : GameInput.t) (state : GameState.t) : result :=
  match input with
  | GameInput.Move direction => Continue (GameState.move direction state)
  | GameInput.Quit => Quit
  end.

End Make.

Module GameStep :
    GameStepApi Dimensions Map Direction Position GameInput GameState :=
  Make Dimensions Map Direction Position GameInput GameState.

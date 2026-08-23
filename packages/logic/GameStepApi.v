Require Import Direction GameInput GameState Position.

Module Type GameStepApi.

Inductive result : Set :=
| Continue (state : GameState.t)
| Quit.

Parameter step : GameInput.t -> GameState.t -> result.

Axiom continuePlayerInBounds :
  forall (input : GameInput.t) (state next : GameState.t),
    step input state = Continue next ->
    Position.in_bounds (GameState.player next).

End GameStepApi.

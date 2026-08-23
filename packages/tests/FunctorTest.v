From Stdlib Require Import BinPos.
From Gimlight.Logic Require Import Logic.

Module AlternativeDimensions : DimensionsApi.

Record t : Set := {
  width : positive;
  height : positive
}.

End AlternativeDimensions.

Module AlternativeDirection : DirectionApi.

Inductive t : Set :=
| left
| right
| up
| down.

End AlternativeDirection.

Module AlternativeMap : MapApi AlternativeDimensions :=
  MakeMap AlternativeDimensions.

Module AlternativePosition :
    PositionApi AlternativeDimensions AlternativeMap AlternativeDirection :=
  MakePosition AlternativeDimensions AlternativeMap AlternativeDirection.

Module AlternativeGameInput : GameInputApi AlternativeDirection :=
  MakeGameInput AlternativeDirection.

Module AlternativeGameState :
    GameStateApi AlternativeDimensions AlternativeMap AlternativeDirection
      AlternativePosition :=
  MakeGameState AlternativeDimensions AlternativeMap AlternativeDirection
    AlternativePosition.

Module AlternativeGameStep :
    GameStepApi AlternativeDimensions AlternativeMap AlternativeDirection
      AlternativePosition AlternativeGameInput AlternativeGameState :=
  MakeGameStep AlternativeDimensions AlternativeMap AlternativeDirection
    AlternativePosition AlternativeGameInput AlternativeGameState.

Theorem alternative_initial_player_is_in_bounds :
  AlternativePosition.in_bounds
    (AlternativeGameState.player AlternativeGameState.initialState).
Proof.
  apply AlternativeGameState.playerInBounds.
Qed.

Theorem alternative_continue_player_is_in_bounds :
  forall (input : AlternativeGameInput.t)
    (state next : AlternativeGameState.t),
    AlternativeGameStep.step input state =
      AlternativeGameStep.Continue next ->
    AlternativePosition.in_bounds
      (AlternativeGameState.player next).
Proof.
  apply AlternativeGameStep.continuePlayerInBounds.
Qed.

Print Assumptions Position.centeredOn_in_bounds.
Print Assumptions Position.move_preserves_bounds.
Print Assumptions GameState.playerInBounds.
Print Assumptions GameStep.continuePlayerInBounds.

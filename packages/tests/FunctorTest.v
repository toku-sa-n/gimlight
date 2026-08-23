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
| Left
| Right
| Up
| Down.

End AlternativeDirection.

Module AlternativeMap : MapApi AlternativeDimensions :=
  Gimlight.Logic.Map.Make AlternativeDimensions.

Module AlternativePosition :
    PositionApi AlternativeDimensions AlternativeMap AlternativeDirection :=
  Gimlight.Logic.Position.Make AlternativeDimensions AlternativeMap
    AlternativeDirection.

Module AlternativeGameInput : GameInputApi AlternativeDirection :=
  Gimlight.Logic.GameInput.Make AlternativeDirection.

Module AlternativeGameState :
    GameStateApi AlternativeDimensions AlternativeMap AlternativeDirection
      AlternativePosition :=
  Gimlight.Logic.GameState.Make AlternativeDimensions AlternativeMap
    AlternativeDirection AlternativePosition.

Module AlternativeGameStep :
    GameStepApi AlternativeDimensions AlternativeMap AlternativeDirection
      AlternativePosition AlternativeGameInput AlternativeGameState :=
  Gimlight.Logic.GameStep.Make AlternativeDimensions AlternativeMap
    AlternativeDirection AlternativePosition AlternativeGameInput
    AlternativeGameState.

Theorem alternative_initial_player_is_in_bounds :
  AlternativePosition.in_bounds
    (AlternativeGameState.player AlternativeGameState.initialState).
Proof.
  apply AlternativeGameState.playerInBounds.
Qed.

Print Assumptions Position.centeredOn_in_bounds.
Print Assumptions Position.move_preserves_bounds.
Print Assumptions GameState.playerInBounds.

From Stdlib Require Import Extraction ExtrOcamlNatInt ExtrOcamlZBigInt.
From Gimlight.Logic Require Export Logic.

Extraction Language OCaml.

Extraction "gimlight_logic.ml"
  Dimensions.t
  Direction.t
  GameInput.t
  Map.t
  Map.default
  Position.t
  Position.x_value
  Position.y_value
  Position.centeredOn
  Position.move
  GameState.t
  GameState.initialState
  GameState.move
  GameStep.result
  GameStep.step.

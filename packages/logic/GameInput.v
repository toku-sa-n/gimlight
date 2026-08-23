Require Import Direction DirectionApi GameInputApi.

Module MakeGameInput (Direction : DirectionApi) : GameInputApi Direction.

Inductive t : Set :=
| Move (direction : Direction.t)
| Quit.

End MakeGameInput.

Module GameInput : GameInputApi Direction :=
  MakeGameInput Direction.

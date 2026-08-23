Require Import Direction DirectionApi GameInputApi.

Module Make (Direction : DirectionApi) : GameInputApi Direction.

Inductive t : Set :=
| Move (direction : Direction.t)
| Quit.

End Make.

Module GameInput : GameInputApi Direction :=
  Make Direction.

Require Import Direction GameInputApi.

Module GameInput : GameInputApi.

Inductive t : Set :=
| Move (direction : Direction.t)
| Quit.

End GameInput.

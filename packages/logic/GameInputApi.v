Require Import Direction.

Module Type GameInputApi.

Inductive t : Set :=
| Move (direction : Direction.t)
| Quit.

End GameInputApi.

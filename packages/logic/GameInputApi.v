Require Import DirectionApi.

Module Type GameInputApi (Direction : DirectionApi).

Inductive t : Set :=
| Move (direction : Direction.t)
| Quit.

End GameInputApi.

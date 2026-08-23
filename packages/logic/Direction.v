Require Import DirectionApi.

Module Direction : DirectionApi.

Inductive t : Set :=
| Rocq_left
| Rocq_right
| Rocq_up
| Rocq_down.

End Direction.

Require Import DirectionApi.

Module Direction : DirectionApi.

Inductive t : Set :=
| left
| right
| up
| down.

End Direction.

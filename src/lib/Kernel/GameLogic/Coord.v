Set Default Goal Selector "!".

From Coq Require Import NArith.

Local Open Scope N_scope.

Definition t := (N * N)%type.

Definition adjacent (p1 p2 : Coord.t) : Prop :=
  match p1, p2 with
  | (x1, y1), (x2, y2) =>
    x1 = x2 /\ y1 = y2 + 1 \/
    x1 = x2 /\ y1 = y2 - 1 \/ x1 = x2 + 1 /\ y1 = y2 \/ x1 = x2 - 1 /\ y1 = y2
  end.

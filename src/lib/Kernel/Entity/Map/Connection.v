Set Default Goal Selector "!".

From Coq Require Import NArith.

From Entity Require Import Map.

Definition in_bounds {width height} (map : t width height) (x y : N) : Prop :=
  (x < N.pos width) /\ (y < N.pos height).

Definition adjacent (x1 y1 x2 y2 : N) : Prop :=
  (x1 = x2 /\ y1 = y2 + 1) \/
  (x1 = x2 /\ y1 = y2 - 1) \/
  (x1 = x2 + 1 /\ y1 = y2) \/
  (x1 = x2 - 1 /\ y1 = y2).

Program Definition can_move
  {width height}
  (map : t width height)
  (sx sy dx dy : N) 
  (H1 : in_bounds map sx sy)
  (H2 : in_bounds map dx dy) : Prop :=
  adjacent sx sy dx dy /\ get_at dx dy _ _ map = false.
Next Obligation.
  unfold in_bounds in *.
  now destruct H2.
Qed.
Next Obligation.
  unfold in_bounds in *.
  now destruct H1.
Qed.


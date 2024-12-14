Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.

From Coquill Require Import FixedSizeArray.

Open Scope N_scope.

Definition t (width height : positive) : Type := FixedSizeArray.t (FixedSizeArray.t bool (N.pos height)) (N.pos width).

Definition all_wall_map (width height : positive) : t width height :=
  FixedSizeArray.make_matrix (N.pos width) (N.pos height) true.

Program Definition build_horizontal_road
  {width height : positive}
  (x_from x_to : N)
  (y : N)
  (x_spec : x_from < x_to <= N.pos width)
  (y_spec : y < N.pos height)
  (map : t width height) : t width height :=
  FixedSizeArray.map_range x_from x_to (FixedSizeArray.update_nth y false _) _ map.

Program Definition build_vertical_road
  {width height : positive}
  (x : N)
  (y_from y_to : N)
  (x_spec : x < N.pos width) 
  (y_spec : y_from < y_to <= N.pos height)
  (map : t width height) : t width height :=
  FixedSizeArray.map_nth x (FixedSizeArray.update_range y_from y_to false _) _ map.

Program Definition initial_map : t 80 50 :=
  build_vertical_road 40 10 30 _ _ (build_horizontal_road 10 50 25 _ _ (all_wall_map 80 50)).
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.

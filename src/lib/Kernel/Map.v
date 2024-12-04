Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.

From Coquill Require Import PArith.

From Coquill Require HalfOpenRange.
From Coquill Require NonEmptyArray.

Open Scope N_scope.

Definition t (width height : positive) : Type := NonEmptyArray.t (NonEmptyArray.t bool width) height.

Definition all_wall_map (width height : positive) : t width height :=
  NonEmptyArray.repeat (NonEmptyArray.repeat true width) height.

Program Definition build_horizontal_road
  {width height : positive}
  (y : N)
  (x : HalfOpenRange.t)
  (map : t width height)
  (y_spec : y < N.pos height)
  (x_spec : HalfOpenRange.upper x <= N.pos width) : t width height :=
  NonEmptyArray.map_nth map y (fun row => NonEmptyArray.update_range row x false x_spec) _.

Program Definition build_vertical_road
  {width height : positive}
  (y : HalfOpenRange.t)
  (x : N)
  (map : t width height)
  (y_spec : HalfOpenRange.upper y <= N.pos height)
  (x_spec : x < N.pos width) : t width height :=
  NonEmptyArray.update_range map y (fun row => NonEmptyArray.update row x false _) _.

Program Definition initial_map : t 80 50 :=
  build_horizontal_road 25 (HalfOpenRange.make 10 50 _) (all_wall_map 80 50) _ _.
Next Obligation.
Proof.
  lia.
Qed.
Next Obligation.
Proof.
  lia.
Qed.

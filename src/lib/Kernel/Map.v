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
  (x : HalfOpenRange.t)
  (y : N)
  (map : t width height)
  (x_spec : HalfOpenRange.upper x <= N.pos width) 
  (y_spec : y < N.pos height) : t width height :=
  NonEmptyArray.map_nth map y (fun row => NonEmptyArray.update_range row x false x_spec) _.

Program Definition build_vertical_road
  {width height : positive}
  (x : N)
  (y : HalfOpenRange.t)
  (map : t width height)
  (x_spec : x < N.pos width) 
  (y_spec : HalfOpenRange.upper y <= N.pos height) : t width height :=
  NonEmptyArray.map_range map y (fun row => NonEmptyArray.update row x false _) _.

Program Definition initial_map : t 80 50 :=
  build_vertical_road 40 (HalfOpenRange.make 10 30 _) 
  (build_horizontal_road (HalfOpenRange.make 10 50 _) 25 (all_wall_map 80 50) _ _) _ _.
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

Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Coq Require Import Lia.
From Coquill Require Import List.
Import ListNotations.

Open Scope Z_scope.

Record game_map : Type := mk_game_map {
  (* `true` denotes wall *)
  map : list (list bool);

  map_is_non_empty : map <> nil;
  map_rows_are_non_empty : forall (row : list bool), In row map -> row <> nil;
  all_rows_have_same_length : forall (row1 row2 : list bool),
    In row1 map -> In row2 map -> length row1 = length row2;
}.

Definition make_all_wall_cells (width height : positive) : list (list bool) :=
  repeat_pos (repeat_pos true width) height.

Lemma make_all_wall_cells_non_empty : forall (width height : positive),
  make_all_wall_cells width height <> nil.
Proof. 
  unfold make_all_wall_cells.
  intros width height H.
  destruct height; simpl in H; inversion H.
  apply app_eq_nil in H1.
  destruct H1.
  apply repeat_pos_non_empty in H0.
  destruct H0.
Qed.

Lemma make_all_wall_cells_rows_are_non_empty : forall (width height : positive) (row : list bool),
  In row (make_all_wall_cells width height) -> row <> nil.
Proof.
  unfold make_all_wall_cells.
  intros width height row H H1.
  rewrite H1 in H.
  apply repeat_pos_spec in H.
  apply repeat_pos_non_empty with (x := true) (n := width).
  auto.
Qed.

Lemma make_all_wall_cells_all_rows_have_same_length : forall (width height : positive) (row1 row2 : list bool),
  In row1 (make_all_wall_cells width height) -> In row2 (make_all_wall_cells width height) -> length row1 = length row2.
Proof.
  unfold make_all_wall_cells.
  intros width height row1 row2 H H1.
  apply repeat_pos_spec in H.
  apply repeat_pos_spec in H1.
  rewrite H, H1.
  reflexivity.
Qed.

Definition all_wall_map (width height : positive) : game_map :=
  mk_game_map (make_all_wall_cells width height)
              (make_all_wall_cells_non_empty width height)
              (make_all_wall_cells_rows_are_non_empty width height)
              (make_all_wall_cells_all_rows_have_same_length width height).

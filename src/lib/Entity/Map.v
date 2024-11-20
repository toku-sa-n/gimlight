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

Definition make_all_wall_cells (width height : Z) : list (list bool) :=
  repeat (repeat true (Z.to_nat width)) (Z.to_nat height).

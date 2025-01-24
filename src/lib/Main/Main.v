Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import Run.
From Controller Require Import EventHandler.

From RAMStore Require MapStore.

Definition main : IO.t unit :=
  let map_repository := MapStore.make 100 100 in
  run map_repository select_button_handler.

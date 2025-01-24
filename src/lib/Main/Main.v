Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import Run.
From Controller Require Import EventHandler.

From RAMStore Require MapStore.

From UsecaseImpl Require GameInitializationUsecaseImpl.

Definition main : IO.t unit :=
  let map_repository := MapStore.make 100 100 in
  let game_initialization_usecase :=
    GameInitializationUsecaseImpl.make map_repository in
  run (select_button_handler game_initialization_usecase).

Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Coquill Require IO.

From Controller Require Import EventHandler.

From RAMStore Require Import MapStore.

From Usecase Require GameInitializationUsecase.

#[local]
Open Scope positive_scope.

Axiom run : 
  forall 
    (map_repository : MapRepository.t 100 100)
    (enter_handler : MapRepository.t 100 100 -> IO.t (GameInitializationUsecase.output 100 100)),
    IO.t unit.

Definition main : IO.t unit := 
  let map_repository := MapStore.make 100 100 in
  run map_repository select_button_handler.

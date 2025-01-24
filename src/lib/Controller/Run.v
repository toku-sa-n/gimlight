Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import EventHandler.

From Usecase Require GameInitializationUsecase.

#[local]
Open Scope positive_scope.

Axiom run : 
  forall 
    (select_button_handler : unit -> IO.t (GameInitializationUsecase.output 100 100)),
    IO.t unit.

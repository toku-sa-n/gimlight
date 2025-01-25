Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import EventHandler.

From Usecase Require FetchCurrentGameStatusUsecase.

From Presenter Require Refresher.

#[local]
Open Scope positive_scope.

Axiom run : 
  forall 
    (select_button_handler : unit -> IO.t unit)
    (refresher : Refresher.t 100 100),
    IO.t unit.

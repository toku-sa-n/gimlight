Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import EventHandler.

From Usecase Require GameInitializationUsecase.

#[local]
Open Scope positive_scope.

Axiom run : 
  forall 
    (map_repository : MapRepository.t 100 100)
    (enter_handler : MapRepository.t 100 100 -> IO.t (GameInitializationUsecase.output 100 100)),
    IO.t unit.

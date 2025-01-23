Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Repository Require MapRepository.

From Usecase Require GameInitializationUsecase.

Definition select_button_handler (map_repository : MapRepository.t 100 100)
  : IO.t (GameInitializationUsecase.output 100 100) :=
  let usecase := GameInitializationUsecase.make map_repository in
  GameInitializationUsecase.execute usecase.

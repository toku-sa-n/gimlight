Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Repository Require MapRepository.

From Usecase Require GameInitializationUsecase.

Definition select_button_handler
  (usecase : GameInitializationUsecase.t 100 100)
  (_ : unit)
  : IO.t (GameInitializationUsecase.output 100 100) :=
  usecase.(GameInitializationUsecase.execute) tt.

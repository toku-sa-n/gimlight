Set Default Goal Selector "!".

From Coq Require Import PArith.
From Coq Require Import NArith.

From Repository Require MapRepository.

From Usecase Require FetchCurrentGameStatusUsecase.

Definition select_button_handler (_ : unit) : IO.t unit := IO.ret tt.

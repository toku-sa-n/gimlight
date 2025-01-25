Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Presenter Require Refresher.
From Presenter Require ViewModel.

From Usecase Require FetchCurrentGameStatusUsecase.

Definition make
  (usecase : FetchCurrentGameStatusUsecase.t 100 100)
  : Refresher.t 100 100
:= {|
  Refresher.refresh := fun f => 
    IO.bind
      (FetchCurrentGameStatusUsecase.execute usecase tt)
      (fun map =>
        match map with
        | FetchCurrentGameStatusUsecase.current_map m => f m
        end);
|}.

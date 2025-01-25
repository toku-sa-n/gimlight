Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import PArith.

From Coquill Require Import CArray.
From Coquill Require IO.

From Repository Require Import MapRepository.

From Usecase Require FetchCurrentGameStatusUsecase.

Canonical t
  {width height : positive}
  (map_repository : MapRepository.t width height)
  : FetchCurrentGameStatusUsecase.t width height
:= {|
  FetchCurrentGameStatusUsecase.execute := fun tt => IO.bind (MapRepository.get map_repository tt) (fun m => IO.ret (FetchCurrentGameStatusUsecase.current_map m));
|}.

Definition make {width height : positive} (repository : MapRepository.t width height) : FetchCurrentGameStatusUsecase.t width height :=
  t repository.

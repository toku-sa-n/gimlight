Set Default Goal Selector "!".

From Coq Require Import NArith.
From Coq Require Import PArith.

From Coquill Require Import FixedSizeArray.
From Coquill Require IO.

From Repository Require Import MapRepository.

From Usecase Require GameInitializationUsecase.

Canonical t
  {width height : positive}
  (map_repository : MapRepository.t width height)
  : GameInitializationUsecase.t width height
:= {|
  GameInitializationUsecase.execute := fun tt => IO.bind (MapRepository.get map_repository tt) (fun m => IO.ret (GameInitializationUsecase.initial_map m));
|}.

Definition make {width height : positive} (repository : MapRepository.t width height) : GameInitializationUsecase.t width height :=
  t repository.

Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Presenter Require ViewModel.

Structure t (width height : positive) := {
  refresh : (ViewModel.t -> IO.t unit) -> IO.t unit;
}.

Arguments refresh {width height} _.

Set Default Goal Selector "!".

From Coquill Require IO.

From Coq Require Import PArith.

From GameLogic Require Map.

Structure t (width height : positive) := {
  get : unit -> IO.t (GameLogic.Map.t width height);
}.

Arguments get {width height} _.

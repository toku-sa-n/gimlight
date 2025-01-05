Set Default Goal Selector "!".

From Coq Require Import PArith.

From GameLogic Require Map.

Structure t (width height : positive) := {
  get : unit -> GameLogic.Map.t width height;
}.

Arguments get {width height} _.

Set Default Goal Selector "!".

From Coq Require Import PArith.

From Entity Require Map.

Structure t (width height : positive) := {
  get : unit -> Entity.Map.t width height;
}.

Arguments get {width height} _.

Set Default Goal Selector "!".

From Coq Require Import PArith.

From Repository Require MapRepository.

Axiom make : forall (width height : positive), MapRepository.t width height.

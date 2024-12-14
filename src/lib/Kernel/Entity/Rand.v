Set Default Goal Selector "!".

From Coq Require Import NArith.

#[local]
Open Scope N_scope.

Structure t := {
  seed :> Type;

  N_range : N -> N -> seed -> (N * seed);
  N_range_spec : forall seed lower upper,
    let (result, _) := N_range lower upper seed in
    lower <= result < upper;

  bool : seed -> (bool * seed);
}.

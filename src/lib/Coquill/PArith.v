From Coq Require Export PArith.

#[local]
Open Scope positive_scope.

Create HintDb positive.

Theorem double_0 : forall n : positive, n + n = n~0.
Proof.
  induction n; try auto.
  - simpl.
    f_equal.
    rewrite Pos.add_carry_spec.
    rewrite IHn.
    reflexivity.
  - simpl.
    f_equal.
    auto.
Qed.

Hint Resolve double_0 : positive.

Theorem double_succ : forall n : positive, Pos.succ (n + n) = n~1.
Proof.
  induction n; try auto.
  - simpl.
    f_equal.
    rewrite Pos.add_carry_spec.
    auto.
  - simpl.
    f_equal.
    auto with positive.
Qed.

Hint Resolve double_succ : positive.

From Coq Require Export PArith.

#[local]
Open Scope positive_scope.

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

Theorem double_succ : forall n : positive, Pos.succ (n + n) = n~1.
Proof.
  induction n; try auto.
  - simpl.
    f_equal.
    rewrite Pos.add_carry_spec.
    rewrite IHn.
    reflexivity.
  - simpl.
    f_equal.
    apply double_0.
Qed.

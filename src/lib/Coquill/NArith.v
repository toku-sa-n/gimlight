Set Default Goal Selector "!".

From Coq Require Export NArith.

From Coquill Require Import PArith.

Open Scope N_scope.

Theorem n_succ_m_pos_1 : forall (n : N) m, n = N.pos (xO m) -> N.succ n = N.pos (xI m).
Proof.
  intros.
  rewrite H.
  reflexivity.
Qed.

Theorem n_succ_m_pos_0 : forall (n : N) m, n = N.pos (Pos.pred (xO m)) -> N.succ n = N.pos (xO m).
Proof.
  intros.
  rewrite H.
  induction m.
  - auto.
  - simpl.
    f_equal.
    rewrite Pos.succ_pred_double.
    auto.
  - auto.
Qed.

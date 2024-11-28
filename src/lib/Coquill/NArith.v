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

Theorem n_pos_1_m_succ : forall n (m : N), N.pos (xI n) <= N.succ m -> N.pos (xO n) <= m.
Proof.
  unfold N.succ.
  intros.
  induction m.
  - destruct n; auto.
  - destruct n; apply N.succ_le_mono; simpl; auto.
Qed.

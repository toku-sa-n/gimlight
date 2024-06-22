Set Default Goal Selector "!".

Require Import Coq.ZArith.ZArith.
Require Import Lia.

Record game_model : Type := mk_game_model {
  counter : Z;
  counter_is_never_negative : Z.ge counter 0;
}.

Lemma ge_refl : forall (n : Z), Z.ge n n.
Proof.
  lia.
Qed.

Definition init_game_model : game_model := mk_game_model 0%Z (ge_refl 0).

Definition get_count (gm : game_model) : Z :=
  match gm with
  | mk_game_model counter _ => counter
  end.

Lemma ge_succ : forall (n m : Z), Z.ge n m -> Z.ge (n + 1) m.
Proof.
  intros.
  simpl.
  lia.
Qed.

Definition increment (gm : game_model) : game_model :=
  match gm with
  | mk_game_model counter f => mk_game_model (counter + 1) (ge_succ counter 0 f)
  end.

Theorem initial_counter_is_zero : get_count init_game_model = 0%Z.
Proof.
  reflexivity.
Qed.

Theorem increment_counter :
  forall (gm : game_model), get_count (increment gm) = (get_count gm + 1)%Z.
Proof.
  intros.
  destruct gm.
  reflexivity.
Qed.

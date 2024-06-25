Set Default Goal Selector "!".

Require Import Coq.ZArith.ZArith.

Inductive game_model : Type :=
  | mk_game_model (counter : Z).

Definition init_game_model : game_model := mk_game_model 0.

Definition increment (gm : game_model) : game_model :=
  match gm with
  | mk_game_model counter => mk_game_model (counter + 1)
  end.

Definition get_count (gm : game_model) : Z :=
  match gm with
  | mk_game_model counter => counter
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

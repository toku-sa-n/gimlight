Inductive game_model : Type :=
  | mk_game_model (counter : nat).

Definition init_game_model : game_model := mk_game_model 0.

Definition increment (gm : game_model) : game_model :=
  match gm with
  | mk_game_model counter => mk_game_model (counter + 1)
  end.

Definition get_count (gm : game_model) : nat :=
  match gm with
  | mk_game_model counter => counter
  end.

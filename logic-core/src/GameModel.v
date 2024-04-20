Inductive GameModel : Type :=
  | MkGameModel (count : nat).

Definition init_game_model : GameModel := MkGameModel 0.

Definition increment (m : GameModel) : GameModel :=
  match m with
  | MkGameModel n => MkGameModel (n + 1)
  end.

Definition get_count (m : GameModel) : nat :=
  match m with
  | MkGameModel n => n
  end.

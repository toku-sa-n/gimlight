Require Import Direction GameInput GameState GameStepApi Position.

Module GameStep : GameStepApi.

Inductive result : Set :=
| Continue (state : GameState.t)
| Quit.

Definition step (input : GameInput.t) (state : GameState.t) : result :=
  match input with
  | GameInput.Move direction => Continue (GameState.move direction state)
  | GameInput.Quit => Quit
  end.

Theorem continuePlayerInBounds :
  forall (input : GameInput.t) (state next : GameState.t),
    step input state = Continue next ->
    Position.in_bounds (GameState.player next).
Proof.
  intros input state next.
  destruct input.
  - simpl. intros equality. inversion equality.
    apply GameState.playerInBounds.
  - simpl. discriminate.
Qed.

End GameStep.

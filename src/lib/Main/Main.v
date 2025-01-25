Set Default Goal Selector "!".

From Coq Require Import PArith.

From Coquill Require IO.

From Controller Require Import Run.
From Controller Require Import EventHandler.

From RAMStore Require MapStore.

From TerminalPresenter Require TerminalRefresher.

From UsecaseImpl Require FetchCurrentGameStatusUsecaseImpl.

Definition main : IO.t unit :=
  let map_repository := MapStore.make 100 100 in
  let fetch_current_game_status_usecase :=
    FetchCurrentGameStatusUsecaseImpl.make map_repository in

  let refresher := TerminalRefresher.make fetch_current_game_status_usecase in
  run
    select_button_handler
    refresher.

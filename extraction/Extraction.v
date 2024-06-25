From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlZBigInt.

From Gimlight Require Import GameModel.

Extraction Language OCaml.

Separate Extraction game_model init_game_model increment get_count.

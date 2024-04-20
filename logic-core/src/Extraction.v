Require Coq.extraction.Extraction.
Require Coq.extraction.ExtrHaskellNatInt.

From Gimlight Require Import GameModel.

Set Extraction Output Directory "src".

Extraction Language Haskell.

Extraction "GimlightLogicCore.hs" Gimlight.GameModel.GameModel init_game_model increment get_count.

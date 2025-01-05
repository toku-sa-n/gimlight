Set Default Goal Selector "!".

From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlZBigInt.

From GameLogic Require Map.

(* We need the `Repository` repetition to avoid a name conflict with the `Map`
   module from `Entity`.

   For consistency, we also append the `Usecase` repetition. *)
From Repository Require MapRepository.

From Usecase Require Import GameInitializationUsecase.

Extraction Language OCaml.

Separate Extraction
  GameLogic.Map.initial_map

  Repository.MapRepository.t

  Usecase.GameInitializationUsecase.t
  Usecase.GameInitializationUsecase.make
  Usecase.GameInitializationUsecase.output
  Usecase.GameInitializationUsecase.execute.

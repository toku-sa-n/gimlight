Set Default Goal Selector "!".

From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlZBigInt.

From Coquill Require IO.

From Controller Require Import Run.

From Main Require Import Main.

From GameLogic Require Map.

From Usecase Require GameInitializationUsecase.

Extraction Language OCaml.

Extract Constant Controller.Run.run => "Controller.Run.run".
Extract Constant RAMStore.MapStore.make => "
  (fun width height ->
    let store = ref Map.initial_map in

    fun () -> !store
    )
  ".

Extract Constant IO.t "'a" => "'a".
Extract Constant IO.ret => "(fun x -> x)".
Extract Constant IO.bind => "(fun v f -> f v)".

Separate Extraction 
  Map.initial_map
  Main.Main.main.

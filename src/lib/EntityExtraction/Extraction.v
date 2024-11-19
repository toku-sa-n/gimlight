From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlZBigInt.

From Entity Require GameModel.

Extraction Language OCaml.

(*
  Note: We use `Extraction` instead of `Separate Extraction` due to current limitations in Dune's handling of nested modules. 
  Specifically, Dune's `coq.extraction` stanza does not support specifying modules located in subdirectories within the `extracted_modules` field. 
  This limitation is documented in the Dune issue tracker: https://github.com/ocaml/dune/issues/10679

  To maintain a manageable project structure without flattening our directory hierarchy, we opt to extract all definitions into a single OCaml file. 
  This approach simplifies the build process and avoids potential complications associated with unsupported nested module extractions.
*)
Extraction "Entity.ml" GameModel.init_game_model GameModel.increment GameModel.get_count.

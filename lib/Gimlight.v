Require Coq.extraction.Extraction.
Require Coq.extraction.ExtrOcamlString.

Require Import String.

Definition greetings:="Hello, World!"%string.

Extraction "gimlight.ml" greetings.

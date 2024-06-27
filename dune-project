(lang dune 3.16)

(name gimlight)

(using coq 0.9)

(generate_opam_files true)

(source
 (github toku-sa-n/gimlight))

(authors "Hiroki Tokunaga")

(maintainers "Hiroki Tokunaga")

(license LICENSE)

(documentation https://github.com/toku-sa-n/gimlight/blob/main/README.md)

(package
 (name gimlight)
 (synopsis "A roguelike game")
 (description "A roguelike game written in Coq")
 (depends
  bogue
  ; XXX: This is a temporary workaround for `opam` ignoring dependency
  ; constraints. Without this workaround, it will install `ctypes-foreign`
  ; 0.22.0 which is incompatible with `tsdl` 0.9.8.
  ; https://github.com/ocaml/opam/issues/5773 might be relevant.
  (ctypes-foreign
   (= 0.18.0))
  dune
  ocaml
  (tsdl
   (= 0.9.8))
  zarith)
 (tags
  (topics "game")))

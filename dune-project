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
 (depends ocaml dune coq)
 (tags
  (topics "game")))

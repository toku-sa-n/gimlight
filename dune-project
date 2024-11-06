(lang dune 3.16)

(name gimlight)

(using coq 0.9)

(generate_opam_files true)

(source
 (github toku-sa-n/gimlight))

(authors "Hiroki Tokunaga")

(maintainers "Hiroki Tokunaga")

(license AGPL-3.0-or-later)

(documentation https://github.com/toku-sa-n/gimlight/blob/main/README.md)

(package
 (name gimlight)
 (synopsis "A roguelike game")
 (description "A roguelike game written in Coq")
 (depends
  dune
  ; We use `lambda-term` because `minttea` requires `ocaml >= 5`, and
  ; `notty` and `curses` are not updated for a few years.
  lambda-term
  lwt
  (ocaml
   (>= 4.14.1))
  zarith)
 (tags
  (topics "game")))

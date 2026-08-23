# Repository Guidelines

## Project Structure & Module Organization

This is a Rocq and OCaml project built with Dune. The verified game rules live
in `packages/logic/`: the Rocq modules define positive map dimensions,
dependent bounded positions, and the game state, while `extraction/` generates
the OCaml module used by the rest of the application. `packages/loop/` contains
the host-independent OCaml view, input, and game loop. `packages/terminal/`
contains the Unix raw-mode and ANSI terminal host. `packages/app/` contains the
executable entry point, and `packages/tests/` contains Rocq assumption checks
and OCaml tests.

## Build, Test, and Development Commands

The repository uses the Rocq/OCaml toolchain from the active opam switch. Run
commands through that switch when its binaries are not already on `PATH`:

- `opam exec -- dune build`: build Rocq proofs, extracted OCaml, libraries, and the executable.
- `opam exec -- dune runtest`: run the Rocq test compilation and OCaml tests.
- `opam exec -- dune exec packages/app/gimlight.exe`: run the terminal game.
- `opam exec -- rocq repl`: inspect definitions or run `Print Assumptions`.

`dune-project` uses Dune's native Rocq language mode and the `rocq.*` build
stanzas. Do not reintroduce Lake, Lean, Rust, or C build configuration.

## Rocq and OCaml Style

Use `positive` for dimensions and keep coordinate bounds in dependent types.
Constructors and smart constructors should discharge the bounds at creation
time; do not add separate mutable or duplicated proof fields to game state.
Keep named theorems such as `Position.centeredOn_in_bounds`,
`Position.move_preserves_bounds`, and `GameState.playerInBounds` as projections
of those type-level guarantees when they are useful to callers.

Use modules for the major domain types (`Dimensions`, `Map`, `Position`,
`GameState`) and descriptive snake_case file names for OCaml modules. Keep the
loop independent of Unix and keep terminal-specific behavior in
`packages/terminal/`.

Every concrete Rocq `Module` must have a corresponding public `<ModuleName>Api`
`Module Type` and must be declared with `Module <ModuleName> :
<ModuleName>Api.`. Keep implementation details out of the module type. This
requirement also applies to future concrete Rocq modules; `Logic.v`, extraction
files, and test files should not introduce additional module types.

## Testing Guidelines

Rocq proofs should be named theorems, not anonymous examples. Check important
theorems with `Print Assumptions` and ensure they are closed under the global
context. OCaml tests should cover centering, all four boundary conditions,
input-driven loop behavior, and the rendered terminal layout. Terminal
restoration must be implemented with exception-safe cleanup and checked with a
pseudo-terminal when possible.

## Generated Files and Hygiene

Do not commit `_build/`, extracted `.ml`/`.mli` files, `.vo`, `.glob`, or other
build output. Do not edit generated lockfiles or restore any of the removed
Lean, Lake, Rust, or C artifacts.

Use Conventional Commit messages, for example
`refactor: rewrite project in Rocq and OCaml`.

# Repository Guidelines

## Project Structure & Module Organization

This is a Lean 4 project managed by Lake. `lakefile.toml` defines the root library and the `gimlight` executable. `Main.lean` is the executable entry point, and `Gimlight.lean` re-exports public modules. Source modules live under `packages/`: `logic` contains maps, positions, game state, directions, movement rules, and proofs; `loop` contains host-agnostic views, input, host abstractions, and loop implementation; `terminal` contains the IO terminal implementation and its Lean FFI declarations. `Gimlight.Logic` and `Gimlight.Loop` remain aggregate modules for their finer-grained submodules.

Native terminal support lives in `packages/terminal/native/terminal/`. Rust implements terminal operations and exports a static library, while `src/lean_shim.c` bridges Rust symbols to the Lean runtime ABI. The terminal package uses `lakefile.lean` because its `extern_lib` target invokes Cargo and links that native library into the Lean executable.

## Build, Test, and Development Commands

- `lake build`: build the library and executable.
- `lake build --wfail`: build with warnings treated as failures, matching CI.
- `lake exe gimlight`: run the terminal game locally.
- `cd packages/terminal/native/terminal && cargo fmt --check`: check Rust formatting.
- `cd packages/terminal/native/terminal && lake env cargo clippy --locked --all-targets -- -D warnings`: lint Rust with Lean headers available.
- `cd packages/terminal/native/terminal && lake env cargo test --locked`: run Rust tests.
- `git ls-files -z -- '*.c' '*.h' | xargs -0 -r clang-format --dry-run --Werror`: check C formatting.
- `lean_prefix="$(lean --print-prefix)"; git ls-files -z -- '*.c' '*.h' | xargs -0 -r clang-tidy -- -std=c11 -isystem "$lean_prefix/include"`: lint C against Lean headers.

Use the Lean version pinned in `lean-toolchain`. Run the checks relevant to the files changed; run `lake build --wfail` for Lean, Lake, FFI, or cross-language integration changes.

## Lakefile Format

Prefer `lakefile.toml` for declarative Lake settings that can be expressed as simple `package`, `require`, `lean_lib`, `lean_exe`, default target, test driver, or lint driver configuration. Use `lakefile.lean` only when the package needs Lean code for custom build behavior such as `extern_lib`, conditional configuration, or direct Lake API use.

Do not add unnecessary `moreLinkArgs`. Linux-only extra linker arguments should be configured only when a build demonstrates they are required.

## Coding Style & Naming Conventions

### Lean

Use Lean 4 style already present in the repository: two-space indentation inside `do`, `match`, and proof blocks; camelCase for definitions such as `initialState`; PascalCase for structures, classes, and inductives such as `GameState` and `GameInput`. Keep pure game rules in `Gimlight.Logic`, host abstractions in `Gimlight.Loop`, and terminal-specific behavior in `Gimlight.Terminal`.

Prefer direct, idiomatic Lean syntax—such as `if`, `for`, ranges, pattern matching, `try/finally`, constructor shorthand, and direct monadic expressions—when it makes code shorter and clearer. Avoid redundant `do` blocks, lambdas, forwarding helpers, and intermediate collections used only to express straightforward control flow. Continue to use helpers for reused behavior, independent concepts, and complex operations.
Organize modules primarily around one major type (`structure`, `inductive`, or `class`). Closely related helper types, operations, and theorems may remain in the same module when that preserves conceptual cohesion; do not split aliases or minor helpers mechanically.
For one or a few declarations belonging to a type, prefer direct qualified definitions such as `Type.member` instead of wrapping them in a namespace block. Keep a namespace block when it organizes multiple declarations as a cohesive group.

### API Design

`Gimlight.Loop` may internally depend on `Gimlight.Logic`, but it must not re-export it with `public import`; add an explicit `public import Gimlight.Logic` in `Gimlight.lean` only when the logic module is intended to be part of the package public API.
Mark APIs intentionally: reserve `public` for types, operations, theorems, and instances that must be available outside their defining module, and prefer `private` for implementation helpers.
Do not add unused public theorems that merely re-expose invariants maintained by private proof fields; expose them only when they are required as part of the external API.
For proof-valued fields whose types already make their proof role clear, avoid a redundant `Proof` suffix and name the field after the invariant itself.
Keep constructors of types with internal representations private by default. When a read-only field's name and type are part of the public contract, prefer a private constructor with a public field instead of a trivial getter. The private constructor permits field reads while preventing external construction and record updates. Use a private field with a getter when its name, type, or representation must remain changeable. Public members remain appropriate when the data representation itself is an intentional contract, including DTO fields, enumeration constructors, and type class methods.

### Rust, C, and FFI

In Rust, place public interfaces before private implementations and callers before the functions they call when declaration order permits. Do not apply this ordering rule mechanically to Lean or C. Follow the repository's `rustfmt`, Clippy, `clang-format`, and `clang-tidy` configurations.

Keep Lean FFI modules as pure foreign boundaries containing only `@[extern]` declarations. Put numeric code constants, type conversions, error handling, and safe IO wrappers in the consuming Lean modules. Keep C limited to the Lean ABI shim; terminal behavior belongs in Rust.

When writing commands in comments or documentation, use the available command-formatting syntax. If there is no dedicated syntax, wrap commands in backticks.

## Testing Guidelines

There is no separate Lean test directory. Verify deterministic Lean logic with named theorems next to the module they validate, especially for movement and boundary behavior; do not use anonymous `example` blocks as unit tests. Keep game-loop behavior testable through `GameHost` rather than raw IO dependencies.

Put Rust unit tests near their implementation and run `lake env cargo test --locked` from `packages/terminal/native/terminal`. Add integration tests only when behavior crosses module boundaries and cannot be covered clearly by unit tests.

## Commit & Pull Request Guidelines

Use Conventional Commit format for commit messages. Recent history uses short imperative messages with prefixes such as `feat:` and `fix:`. Follow that style: `feat: add inventory state`, `fix: restore terminal on quit`. PRs should describe the behavior change, mention relevant commands run, link related issues when available, and include screenshots or terminal recordings for visible TUI changes.

## Generated Files, File Hygiene, and CI

Do not edit or commit generated build output under `.lake/` or Cargo `target/` directories. The root `lake-manifest.json` and `packages/terminal/native/terminal/Cargo.lock` are generated lockfiles that are tracked; do not edit them by hand, and regenerate them with Lake or Cargo when their dependencies change. Keep CI invocations locked.

CI runs independent checks for the Lean build; Rust formatting, linting, and tests; C formatting and linting; TOML formatting with Taplo; and Markdown, YAML, and JSON formatting with Prettier. It also rejects binary files, CRLF endings, and missing final newlines. Keep committed files text-based and formatted.

When configuring or changing GitHub Actions, keep each job to at most one responsibility and do not mix separate concerns such as build, format checks, or deploy in the same job. Split CI checks into independent jobs that each check out the repository and run without `needs` unless a real dependency requires it.

## Persistent Instructions

Treat requests such as "Memorize ..." or equivalent wording as requests to update durable project instructions, not only as conversational memory. Before changing anything, check existing instructions for duplication or conflicts, then apply the smallest consistent update.

Choose the most appropriate project file for the rule: existing `AGENTS.md`, skill files, subagent files, or another relevant file. If no suitable file exists, create one when needed. You may split, merge, or reorganize instructions so the final rule set remains coherent and avoids unnecessary duplication.

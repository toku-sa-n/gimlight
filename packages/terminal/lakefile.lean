import Lake

open System Lake DSL

package terminal where
  version := v!"0.1.0"

require loop from "../loop"

lean_lib Gimlight.Terminal

extern_lib libgimlight_terminal pkg := do
  let nativeDir := pkg.dir / "native" / "terminal"
  -- Lake clean has no package-specific hook. Put Cargo's target dir under
  -- this package build dir so `lake clean` removes Cargo output as well.
  let targetDir := pkg.buildDir / "cargo"
  let manifest := nativeDir / "Cargo.toml"
  let lockfile := nativeDir / "Cargo.lock"
  let srcDir := nativeDir / "src"
  let libFile :=
    targetDir / "release" / nameToStaticLib "gimlight_terminal"
  let manifestDep ← inputTextFile manifest
  let lockfileDep ← inputTextFile lockfile
  let buildScriptDep ← inputTextFile (nativeDir / "build.rs")
  let leanShimDep ← inputTextFile (srcDir / "lean_shim.c")
  let rustLibDep ← inputTextFile (srcDir / "lib.rs")
  let nativeDeps :=
    #[manifestDep, lockfileDep, buildScriptDep, leanShimDep, rustLibDep]
  buildFileAfterDep libFile (Job.mixArray nativeDeps) fun _ => do
    proc {
      cmd := "cargo"
      args := #[
        "build",
        "--locked",
        "--release",
        "--manifest-path", manifest.toString,
        "--target-dir", targetDir.toString
      ]
    }

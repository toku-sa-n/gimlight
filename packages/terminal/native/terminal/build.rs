use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let lean_include = lean_sysroot().join("include");

    println!("cargo:rerun-if-changed=src/lean_shim.c");
    println!("cargo:rerun-if-env-changed=LEAN_SYSROOT");

    cc::Build::new()
        .file("src/lean_shim.c")
        .include(lean_include)
        .compile("lean_terminal_shim");
}

fn lean_sysroot() -> PathBuf {
    if let Some(sysroot) = env::var_os("LEAN_SYSROOT") {
        return PathBuf::from(sysroot);
    }

    let output = Command::new("lean")
        .arg("--print-prefix")
        .output()
        .expect("failed to run `lean --print-prefix`");
    if !output.status.success() {
        panic!("`lean --print-prefix` failed with status {}", output.status);
    }

    let prefix = String::from_utf8(output.stdout).expect("Lean prefix is not valid UTF-8");
    PathBuf::from(prefix.trim())
}

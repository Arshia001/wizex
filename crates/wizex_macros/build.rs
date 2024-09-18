use std::{env, path::PathBuf};

fn main() {
    let target = env::var("TARGET").unwrap();
    let is_wasi = target.contains("wasi");
    if !is_wasi {
        panic!("wizex_macros can only be built for WASIX targets");
    }

    let wasi_sysroot_path =
        env::var("WASI_SYSROOT").expect("The wasm32-wasi target requires WASI_SYSROOT to be set");
    let wasi_sysroot = PathBuf::from(&wasi_sysroot_path);

    cc::Build::new()
        .cpp(false)
        .compiler("clang")
        .file("src/call_main.c")
        .flag("--target=wasm32-wasi")
        .flag(&format!("--sysroot={}", wasi_sysroot.display()))
        .compile("call_main");

    println!("cargo:rerun-if-changed=src/call_main.c");
}

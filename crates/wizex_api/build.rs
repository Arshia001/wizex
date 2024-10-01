fn main() {
    let target = std::env::var("TARGET").unwrap();
    let is_wasi = target.contains("wasi");
    if !is_wasi {
        return;
    }

    cc::Build::new()
        .cpp(false)
        .compiler("clang")
        .file("src/call_main.c")
        .flag("--target=wasm32-wasi")
        .compile("call_main");

    println!("cargo:rerun-if-changed=src/call_main.c");
}

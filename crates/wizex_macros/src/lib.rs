#[macro_export]
macro_rules! WIZEX_INIT {
    ($init_fn:path) => {
        mod __wizex_support {
            extern "C" {
                fn __wasm_call_ctors();
                fn __wasm_call_dtors();

                fn __wasi_init_tp();
                fn __wasi_proc_exit(r: i32);

                // What we'd like to do is to call __main_void directly here. However,
                // rustc (inconsistently) doesn't like having a declaration for it in
                // Rust code; it interprets the existence of the declaration as having
                // a duplicate `fn main()`, and errors out. As a work-around, we put
                // the actual call to __main_void in call_main.c, and wrap it in a
                // separate function which we can call safely from Rust.
                fn __call_main_void() -> i32;
            }

            #[export_name = "wizex.initialize"]
            pub extern "C" fn __wizex_initialize() {
                unsafe {
                    __wasi_init_tp();
                    __wasm_call_ctors();
                }

                $init_fn();
            }

            #[export_name = "wizex.resume"]
            pub extern "C" fn __wizex_resume() {
                unsafe {
                    let r = __call_main_void();
                    __wasm_call_dtors();
                    if r != 0 {
                        __wasi_proc_exit(r);
                    }
                }
            }
        }
    };
}

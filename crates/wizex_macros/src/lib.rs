#[macro_export]
macro_rules! WIZEX_INIT {
    ($init_fn:path) => {
        mod __wizex_support {
            extern "C" {
                fn __wasm_call_ctors();
                fn __wasm_call_dtors();

                fn __wasi_init_tp();
                fn __wasi_proc_exit(r: i32);

                fn __main_void() -> i32;
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
                    let r = __main_void();
                    __wasm_call_dtors();
                    if r != 0 {
                        __wasi_proc_exit(r);
                    }
                }
            }
        }
    };
}

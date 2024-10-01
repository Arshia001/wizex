use core::{mem::size_of, ptr::addr_of};

const REWIND_STATE_NONE: u8 = 0;
const REWIND_STATE_INITIALIZING: u8 = 1;
const REWIND_STATE_UNWINDING_AFTER_INIT: u8 = 2;
const REWIND_STATE_INIT_COMPLETE: u8 = 3;
const REWIND_STATE_REWINDING_ON_RESUME: u8 = 4;
// TODO: this can be used to support unwinding from other sources, if asyncify lets us call get_state
const REWIND_STATE_REWIND_COMPLETE: u8 = 5;

// The assumption is that the call to finalize_init will come very low on the stack, so
// we can keep this number quite low.
const REWIND_BUFFER_SIZE: usize = 512;

static mut REWIND_STATE: u8 = 0;
static mut REWIND_ARG: [u8; size_of::<usize>() * 2] = [0u8; size_of::<usize>() * 2];
static mut REWIND_BUFFER: [u8; REWIND_BUFFER_SIZE] = [0u8; REWIND_BUFFER_SIZE];

fn set_rewind_arg() {
    unsafe {
        let buffer_start = addr_of!(REWIND_BUFFER) as usize;

        REWIND_ARG[0..size_of::<usize>()].copy_from_slice(buffer_start.to_ne_bytes().as_ref());
        REWIND_ARG[size_of::<usize>()..size_of::<usize>() * 2]
            .copy_from_slice((buffer_start + REWIND_BUFFER_SIZE).to_ne_bytes().as_ref());
    }
}

pub fn finalize_init() {
    unsafe {
        match REWIND_STATE {
            // Nothing to do; this can happen if the code was invoked by running the module
            // directly as opposed to running it through wizex.
            REWIND_STATE_NONE => (),

            // Initialize rewind buffer and start rewinding
            REWIND_STATE_INITIALIZING => {
                REWIND_STATE = REWIND_STATE_UNWINDING_AFTER_INIT;

                set_rewind_arg();
                asyncify::start_unwind(addr_of!(REWIND_ARG) as usize);
            }

            // Reset the rewind buffer and return
            REWIND_STATE_REWINDING_ON_RESUME => {
                asyncify::stop_rewind();
                REWIND_STATE = REWIND_STATE_REWIND_COMPLETE;
            }

            x => {
                panic!("Bad REWIND_STATE value {}", x)
            }
        }
    }
}

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
extern "C" fn __wizex_initialize() {
    unsafe {
        // Record that we're initializing
        REWIND_STATE = REWIND_STATE_INITIALIZING;

        // Call initialization functions
        __wasi_init_tp();
        __wasm_call_ctors();

        // Call main and wait for an unwind to happen
        __call_main_void();

        if REWIND_STATE != REWIND_STATE_UNWINDING_AFTER_INIT {
            panic!("The main function failed to call wizex_macros::finalize_init");
        }

        // Stop unwinding now
        asyncify::stop_unwind();

        REWIND_STATE = REWIND_STATE_INIT_COMPLETE;
    }
}

#[export_name = "wizex.resume"]
extern "C" fn __wizex_resume() {
    unsafe {
        if REWIND_STATE != REWIND_STATE_INIT_COMPLETE {
            panic!("Module was not properly pre-initialized")
        }

        REWIND_STATE = REWIND_STATE_REWINDING_ON_RESUME;

        // Start rewinding the stack, program will resume from the finalize_init call.
        // REWIND_ARG will have the correct values from wizening already.
        asyncify::start_rewind(addr_of!(REWIND_ARG) as usize);

        // From here on out, everything happens like normal
        let r = __call_main_void();

        __wasm_call_dtors();
        if r != 0 {
            __wasi_proc_exit(r);
        }
    }
}

// TODO: asyncify doesn't let us call get_state from within the module yet, see:
// https://github.com/WebAssembly/binaryen/blob/d8c1b0c0ceb4cc4eb59f3f3ab4840636c78e2a44/src/passes/Asyncify.cpp#L569
mod asyncify {
    // pub(super) const ASYNCIFY_STATE_NORMAL: i32 = 0;
    // pub(super) const ASYNCIFY_STATE_UNWINDING: i32 = 1;
    // pub(super) const ASYNCIFY_STATE_REWINDING: i32 = 2;

    #[link(wasm_import_module = "asyncify")]
    extern "C" {
        pub(super) fn start_unwind(arg0: usize);
        pub(super) fn stop_unwind();
        pub(super) fn start_rewind(arg0: usize);
        pub(super) fn stop_rewind();
        // pub(super) fn get_state() -> i32;
    }
}

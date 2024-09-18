// See comments in lib.rs for an explanation of why this exists.

int __main_void();

int __call_main_void() {
    return __main_void();
}

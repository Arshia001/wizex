use std::cell::LazyCell;

struct Test {
    value: u32,
}

static mut ORIG_VALUE: u32 = 0;
static mut T: LazyCell<Test> = LazyCell::new(|| {
    println!("constructing new Test value");
    Test { value: 1 }
});

fn init() {
    unsafe {
        ORIG_VALUE = T.value;
    }
}

fn main() {
    unsafe {
        init();
        wizex_api::finalize_init();
        println!("ORIG_VALUE (should be 1): {}", ORIG_VALUE);
    }
}

use std::cell::LazyCell;

struct Test {
    value: u32,
}

static mut INITIALIZED: bool = false;
static mut ORIG_VALUE: u32 = 0;
static mut T: LazyCell<Test> = LazyCell::new(|| {
    println!("constructing new Test value");
    Test { value: 1 }
});

fn init() {
    unsafe {
        ORIG_VALUE = T.value;
        INITIALIZED = true;
    }
}

fn main() {
    unsafe {
        if !INITIALIZED {
            init();
        }
        println!(
            "argc (should not be baked into snapshot): {}",
            std::env::args().len()
        );
        println!("ORIG_VALUE (should be 1): {}", ORIG_VALUE);
    }
}

wizex_macros::WIZEX_INIT!(crate::init);

WIZEX := ../../target/release/wizex
WASMER ?= wasmer

.PHONY: all
all: main_initialized.wasm

../../target/wasm32-wasmer-wasi/debug/wizex-rust-example.wasm: Cargo.toml src/main.rs
	cargo wasix build

main_initialized.wasm: ../../target/wasm32-wasmer-wasi/debug/wizex-rust-example.wasm
	$(WIZEX) --allow-wasix --wasm-bulk-memory=true -r _start=wizex.resume -o $@ $^

.PHONY: test
test: main_initialized.wasm
	$(WASMER) run $^

.PHONY: clean
clean:
	rm -f *.wasm

WASMER ?= wasmer

.PHONY: all
all: main_initialized.wasm

../../target/wasm32-wasmer-wasi/debug/wizex-rust-example.wasm: Cargo.toml src/main.rs ../../crates/wizex_api/**/*
	cargo wasix build

main_initialized.wasm: ../../target/wasm32-wasmer-wasi/debug/wizex-rust-example.wasm
	cargo run --manifest-path ../../Cargo.toml --all-features --bin wizex \
	  -- \
	  --allow-wasix --wasm-bulk-memory=true -r _start=wizex.resume -o $@ $^
	echo "Initialization complete"

.PHONY: test
test: main_initialized.wasm
	echo "Running pre-initialized module"
	$(WASMER) run $^

.PHONY: clean
clean:
	rm -f *.wasm

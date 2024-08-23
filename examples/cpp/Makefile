CXX := clang++
CXXFLAGS := -O2 -I ../../include/ -target wasm32-wasi --sysroot /opt/wasix-sysroot
WIZEX := ../../target/release/wizex
WASMER ?= wasmer

.PHONY: all
all: main_initialized.wasm

main.wasm: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

main_initialized.wasm: main.wasm
	$(WIZEX) --allow-wasix --wasm-bulk-memory=true -r _start=wizex.resume -o $@ $^

.PHONY: test
test: main_initialized.wasm
	$(WASMER) run $^

.PHONY: clean
clean:
	rm -f *.wasm

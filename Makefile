MODS=main malloc print
OBJECTS=$(MODS:%=build/%.o)

main.wasm: $(OBJECTS)
	wasm-ld -o $@ --no-entry --export-all --import-undefined $^

build/%.o: build/%.ll
	llc -o $@ -march=wasm32 -filetype=obj $<

build/%.ll: %.cc
	clang++ -o $@ --target=wasm32 -mbulk-memory -emit-llvm -O2 -S -MD -std=c++17 -c $<

-include $(OBJECTS:.o=.d)

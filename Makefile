MODS=main print font-data
OBJECTS=$(MODS:%=build/%.cc.o) build/keymap-mac.cc.o
WOBJECTS=$(MODS:%=build/%.w.o) build/malloc.w.o build/keymap-web.w.o

all: main.wasm build/main

build/main: build/main-mac.m.o build/print-float.c.o build/glad.c.o $(OBJECTS)
	clang++ -o $@ -O2 -MD $^ -framework OpenGL -framework Cocoa

build/%.m.o: %.m
	clang -o $@ -O2 -MD -Wno-deprecated-declarations -Wall -Wextra -Wconversion -c $<

main.wasm: $(WOBJECTS)
	wasm-ld -o $@ --no-entry --export-all --import-undefined $^

build/%.w.o: build/%.ll
	llc -o $@ -march=wasm32 -filetype=obj $<

build/%.ll: %.cc
	clang++ -o $@ --target=wasm32 -mbulk-memory -emit-llvm -fno-rtti -O2 -S -MD -std=c++17 -c $<

build/%.cc.o: %.cc
	clang++ -o $@ -fno-rtti -O2 -MD -std=c++17 -c $<

build/%.c.o: %.c
	clang -o $@ -O2 -MD -std=c11 -c $<

-include $(OBJECTS:.o=.d)
-include $(WOBJECTS:.o=.d)

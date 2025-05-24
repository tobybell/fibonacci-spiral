main.wasm: main.cc
	clang++ -o main.ll --target=wasm32 -emit-llvm -O2 -S -std=c++17 -c main.cc
	llc -o main.o -march=wasm32 -filetype=obj main.ll
	wasm-ld -o main.wasm --no-entry --export-all --import-undefined main.o

build:
	@dune build

build_bin:
	@./scripts/build_bin.sh

build_fibonacci: build
	@./_build/default/bin/main.exe examples/fibonacci.tick

run_fibonacci: build_fibonacci build_bin
	@./output

build_asm:
	@llc output.ll -filetype=asm

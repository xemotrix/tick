build:
	@dune build

build_bin:
	@./scripts/build_bin.sh

build_fibonacci:
	@./_build/default/bin/main.exe examples/fibonacci.tick
	@./scripts/build_bin.sh

run_fibonacci: build_fibonacci
	@./output

build_asm:
	@llc output.ll -filetype=asm

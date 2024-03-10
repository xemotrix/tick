build:
	dune build
	cp -f ./_build/default/bin/main.exe tick

codegen: build
	./tick examples/$(EX).tick

intr: codegen
	@printf "\nEXECUTION:\n"
	@lli output.ll

compile: codegen
	llc -filetype=asm output.ll # for debugging
	llc -filetype=obj output.ll -o output.o
	clang output.o -o output
	@rm output.o

run: compile
	@printf "\n"
	./output

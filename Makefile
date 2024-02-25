build:
	dune build
	cp -f ./_build/default/bin/main.exe tick

compile: build
	./tick examples/$(EX).tick
	llc -filetype=asm output.ll # for debugging
	llc -filetype=obj output.ll -o output.o
	clang output.o -o output
	@rm output.o

run: compile
	@printf "\n"
	./output

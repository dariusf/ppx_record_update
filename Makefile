
.PHONY: all

all:
	dune test
	dune exec examples/example.exe
	dune describe pp examples/example.ml | sed -n '/let _ =/,$$p'

GREEN=$(shell tput setaf 2)
RESET=$(shell tput sgr0)

.PHONY: all lol examples test update-tests clean

all:
	dune build @install

lol:
	ocamlc -dparsetree test/test.ml

examples:
	dune build examples/a.exe
	_build/default/.ppx/ppx_record_update/ppx.exe examples/a.ml

test: all
	_build/default/.ppx/ppx_record_update/ppx.exe test/test.ml
	dune runtest
	@echo "$(GREEN)PASSED$(RESET)"

update-tests: all
	cp _build/default/test/test.result test/test.expected
	cp _build/default/test/errors.result test/errors.expected

clean:
	dune clean

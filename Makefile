
GREEN=$(shell tput setaf 2)
RESET=$(shell tput sgr0)

.PHONY: all lol examples test update-tests clean

all:
	jbuilder build @install

lol:
	ocamlc -dparsetree test/test.ml

examples:
	jbuilder build examples/a.exe

test: all
	_build/default/.ppx/ppx_record_update+ppx_driver.runner/ppx.exe test/test.ml
	jbuilder runtest
	@echo "$(GREEN)PASSED$(RESET)"

update-tests: all
	cp _build/default/test/test.result test/test.expected
	cp _build/default/test/errors.result test/errors.expected

clean:
	jbuilder clean

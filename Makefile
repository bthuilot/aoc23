.PHONY: run build test

run:
	@dune exec advent_of_code_2023

build:
	@dune build

test:
	@dune runtest

DAY_NUM ?= $(shell bash -c 'read -s -p "day #: " dn; echo $$dn')

generate_test:
	@echo Day number › $(DAY_NUM)
	@cp test/days/day00_test.ml test/days/day$(DAY_NUM)_test.ml


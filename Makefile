
.PHONY: run build test

run:
	@dune exec advent_of_code_2023

build:
	@dune build

test:
	@dune runtest

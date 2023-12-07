.PHONY: run build test

run:
	@dune exec advent_of_code_2023

build:
	@dune build

test:
	@dune runtest

generate_test:
	@read -p "Enter day #: " DAY; \
	day=$$(printf %02d $$DAY); \
	@cp test/days/day00_test.ml test/days/day$${day}_test.ml; \
	sed -i "s/Day00/Day$${day}/g" test/days/day00_test.mll; \
	sed -i "s/))/ Day$${day}_test))/" test/dune; \
	sed -i "s/\([ \t]*\)]/\1  \"Day $${DAY}\", Day$${day}_test.suite;\n)/" test/aoc23.ml

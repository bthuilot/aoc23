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
	test_file="test/days/day$${day}_test.ml"; \
	cp test/days/day00_test.ml $${test_file}; \
	sed -i '' "s/Day00/Day$${day}/g" $${test_file}; \
	sed -i '' "s/))/ Day$${day}_test))/" test/dune; \
	sed -r -i '' -e "s/([[:space:]]+)\]/\1  \"Day 9\", Day09_test.suite\;\n\1]/" test/aoc23_test.ml


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
	sed -i '' "s/\(\ *\)]/\1  \"Day $${DAY}\", Day$${day}_test.suite;\n)/" test/aoc23_test.ml

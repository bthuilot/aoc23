# Advent of Code 2023 - OCaml

### Index

1. [Building/Running](#Buidling/Running)
2. [Tests](#Tests)
3. [Completed Days](#Completed-Days)

This repositories stores my solutions in OCaml to [Advent of Code 2023](https://adventofcode.com/2023).

For more info on the approach to each day,
read the module header comment located at the top of each day's source file 
(an index is located below)


## Building/Running

This project leverages dune to compile and run the project, to build an executable simply run

```bash
dune exec aoc23
# or
make

# Optionally specify only a subset of days to run
dune exec aoc23 1 8 23 # Run days 1, 8 and 23
```

## Tests

Tests are written using [alcotest](https://github.com/mirage/alcotest).

To test functionaility of the project, invoke the following:

```bash
make test
# Or
dune runtest
```

## Completed Days

Below is an index to every completed day's implementation source code (containing documentation of approach) and the challenge for the day

- [Day 0](lib/days/day00.ml) : *This is a test day supposed to server a placeholder until the challenge starts*
- [Day 1](lib/days/day01.ml) : [Problem](https://adventofcode.com/2023/day/1)
- [Day 2](lib/days/day02.ml) : [Problem](https://adventofcode.com/2023/day/2)
- [Day 3](lib/days/day03.ml) : [Problem](https://adventofcode.com/2023/day/3)
- [Day 4](lib/days/day04.ml) : [Problem](https://adventofcode.com/2023/day/4)
- [Day 5](lib/days/day05.ml) : [Problem](https://adventofcode.com/2023/day/5)
- [Day 6](lib/days/day06.ml) : [Problem](https://adventofcode.com/2023/day/6)


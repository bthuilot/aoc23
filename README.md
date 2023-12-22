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
- [Day 7](lib/days/day07.ml) : [Problem](https://adventofcode.com/2023/day/7)
- [Day 8](lib/days/day08.ml) : [Problem](https://adventofcode.com/2023/day/8)
- [Day 9](lib/days/day09.ml) : [Problem](https://adventofcode.com/2023/day/9)
- [Day 10](lib/days/day10.ml) : [Problem](https://adventofcode.com/2023/day/10)
- [Day 11](lib/days/day11.ml) : [Problem](https://adventofcode.com/2023/day/11)
- [Day 12](lib/days/day12.ml) : [Problem](https://adventofcode.com/2023/day/12)
- [Day 13](lib/days/day13.ml) : [Problem](https://adventofcode.com/2023/day/13)
- [Day 14](lib/days/day14.ml) : [Problem](https://adventofcode.com/2023/day/14)
- [Day 15](lib/days/day15.ml) : [Problem](https://adventofcode.com/2023/day/15)
- [Day 16](lib/days/day16.ml) : [Problem](https://adventofcode.com/2023/day/16)
- [Day 17](lib/days/day17.ml) : [Problem](https://adventofcode.com/2023/day/17)
- [Day 18](lib/days/day18.ml) : [Problem](https://adventofcode.com/2023/day/18)
- [Day 19](lib/days/day19.ml) : [Problem](https://adventofcode.com/2023/day/19)
- [Day 20](lib/days/day20.ml) : [Problem](https://adventofcode.com/2023/day/20) -- part 1 only
- [Day 21](lib/days/day21.ml) : [Problem](https://adventofcode.com/2023/day/21) -- part 1 only
- [Day 22](lib/days/day22.ml) : [Problem](https://adventofcode.com/2023/day/22)

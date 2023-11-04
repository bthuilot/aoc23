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

This project leverages dune (and Make) to compile and run the project, to build an executable simply run

```bash
make
# or 
dune exec advent_of_code
```

## Tests

To test functionaility of the project, There exists a test suite that can be run by invoking

```bash
make test
# Or
dune runtest
```

## Completed Days

Below is an index to every completed day's implementation source code (containing documentation of approach) and the challenge for the day

- [Day 0](lib/day00.ml) : *This is a test day supposed to server a placeholder until the challenge starts*

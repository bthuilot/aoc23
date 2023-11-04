(lang dune 3.7)

(name advent_of_code_2023)

(generate_opam_files true)

(source
 (github bthuilot/aoc23))

(authors "Bryce Thuilot <bryce@thuilot.io>")

(maintainers "Bryce Thuilot <bryce@thuilot.io>")

(license GPL-3)

(homepage https://github.com/bthuilot/aoc23)

(package
 (name advent_of_code_2023)
 (synopsis "Advent of Code '23")
 (description "Implmenetion of Advent of Code 2023 solutions in OCaml")
 (depends ocaml dune core)
 (tags
  (aoc23 "advent of code")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

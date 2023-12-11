open Alcotest

let day = new Advent_of_code_2023.Day10.t

let example_1 = {|.....
.S-7.
.|.|.
.L-J.
.....|}

let example_2 = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let example_3 = {|-L|F7
7S-7|
L|7||
-L-J|
L|-JF|}


let test_part_example1 () =
  let expected = "4" in
  let actual = day#part1 example_1 in
  check string "part1 is valid" expected actual

let test_part_example2 () =
  let expected = "8" in
  let actual = day#part1 example_2 in
  check string "part1 is valid" expected actual


let test_part_example3 () =
  let expected = "4" in
  let actual = day#part1 example_3 in
  check string "part1 is valid" expected actual

let example_4 = {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........|}

let example_5 = {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L|}

let test_part2_example4 () =
  let expected = "4" in
  let actual = day#part2 example_4 in
  check string "part2 is valid" expected actual


let test_part2_example5 () =
  let expected = "10" in
  let actual = day#part2 example_5 in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1 example 1", `Quick, test_part_example1;
    "part1 example 2", `Quick, test_part_example2;
    "part1 example 3", `Quick, test_part_example3;
    "part2 example 4", `Quick, test_part2_example4;
    "part2 example 5", `Quick, test_part2_example5;
  ]



open Alcotest

let day = new Advent_of_code_2023.Day20.t

let example_1 = {|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a|}

let example_2 = {|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output|}

let test_part1_example1 () =
  let expected = "32000000" in
  let actual = day#part1 example_1 in
  check string "part1 is valid" expected actual

let test_part1_example2 () =
  let expected = "11687500" in
  let actual = day#part1 example_2 in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "TEST" in
  let actual = day#part2 example_1 in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1 example1", `Quick, test_part1_example1;
    "part1 example2", `Quick, test_part1_example2;
    "part2", `Quick, test_part2;
  ]



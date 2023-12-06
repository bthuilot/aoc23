open Alcotest

let day = new Advent_of_code_2023.Day07.t

let input = {||}

let test_part1 () =
  let expected = "Not yet published" in
  let actual = day#part1 input  in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "Not yet published" in
  let actual = day#part2 input in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



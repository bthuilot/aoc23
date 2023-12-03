open Alcotest

let day = new Advent_of_code_2023.Day03.t

let input = {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}

let test_part1 () =
  let expected = "4361" in
  let actual = day#part1 input  in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "2286" in
  let actual = day#part2 input in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



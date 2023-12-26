open Alcotest

let day = new Advent_of_code_2023.Day24.t

let i = {|19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3|}

let test_part1 () =
  let old_min = !Advent_of_code_2023.Day24.part1_min in
  Advent_of_code_2023.Day24.part1_min := 7.;
  let old_max = !Advent_of_code_2023.Day24.part1_max in
  Advent_of_code_2023.Day24.part1_max := 27.;
  let expected = "2" in
  let actual = day#part1 i in
  Advent_of_code_2023.Day24.part1_min := old_min;
  Advent_of_code_2023.Day24.part1_max := old_max;
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "TEST" in
  let actual = day#part2 i in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



open Alcotest

let day = new Advent_of_code_2023.Day09.t

let i = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let test_part1 () =
  let expected = "114" in
  let actual = day#part1 i in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "2" in
  let actual = day#part2 i in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



open Alcotest

let day = new Advent_of_code_2023.Day04.t

let input = {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}

let test_part1 () =
  let expected = "13" in
  let actual = day#part1 input  in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "30" in
  let actual = day#part2 input in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



open Alcotest

let day = new Advent_of_code_2023.Day01.t

let input2 = {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixtee|}

let input1 = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet|}

let test_part1 () =
  let expected = "142" in
  let actual = day#part1 input1  in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "281" in
  let actual = day#part2 input2 in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



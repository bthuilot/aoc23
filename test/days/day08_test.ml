open Alcotest

let day = new Advent_of_code_2023.Day08.t

let ex1 = {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}


let ex2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let ex3 = {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)}|}



let test_part1_ex1 () =
  let expected = "2" in
  let actual = day#part1 ex1 in
  check string "part1 is valid" expected actual

let test_part1_ex2 () =
  let expected = "6" in
  let actual = day#part1 ex2 in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "6" in
  let actual = day#part2 ex3 in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1 example 1", `Quick, test_part1_ex1;
    "part1 example 2", `Quick, test_part1_ex2;
    "part2 example 3", `Quick, test_part2;
  ]



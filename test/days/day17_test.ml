open Alcotest

let day = new Advent_of_code_2023.Day17.t

let example_1 = {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|}

let example_2 = {|111111111111
999999999991
999999999991
999999999991
999999999991|}


let test_part1 () =
  let expected = "102" in
  let actual = day#part1 example_1 in
  check string "part1 is valid" expected actual


let test_part2_example1 () =
  let expected = "94" in
  let actual = day#part2 example_1 in
  check string "part2 is valid" expected actual

let test_part2_example2 () =
  let expected = "71" in
  let actual = day#part2 example_2 in
  check string "part2 is valid" expected actual

 
 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2 example 2", `Quick, test_part2_example2;
    "part2 example 1", `Quick, test_part2_example1;
  ]



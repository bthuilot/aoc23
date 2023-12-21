open Alcotest

let day = new Advent_of_code_2023.Day21.t

let i = {|...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........|}

let test_part1 () =
  let expected = "16" in
  let () = Advent_of_code_2023.Day21.part1_steps := 6 in
  let actual = day#part1 i in
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



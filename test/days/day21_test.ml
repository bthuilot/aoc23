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
  let old = !Advent_of_code_2023.Day21.part1_steps in
  let () = Advent_of_code_2023.Day21.part1_steps := 6 in
  let actual = day#part1 i in
  let () = Advent_of_code_2023.Day21.part1_steps := old in
  check string "part1 is valid" expected actual


let test_part2_example1 () =
  let expected = "50" in
  let old = !Advent_of_code_2023.Day21.part2_steps in
  let () = Advent_of_code_2023.Day21.part2_steps := 10 in
  let actual = day#part2 i in
  let () = Advent_of_code_2023.Day21.part2_steps := old in
  check string "part2 example1 is valid" expected actual


let test_part2_example2 () =
  let expected = "167004" in
  let old = !Advent_of_code_2023.Day21.part2_steps in
  let () = Advent_of_code_2023.Day21.part2_steps := 500 in
  let actual = day#part2 i in
  let () = Advent_of_code_2023.Day21.part2_steps := old in
  check string "part2 example2 is valid" expected actual


let test_part2_example3 () =
  let expected = "16733044" in
  let old = !Advent_of_code_2023.Day21.part2_steps in
  let () = Advent_of_code_2023.Day21.part2_steps := 5000 in
  let actual = day#part2 i in
  let () = Advent_of_code_2023.Day21.part2_steps := old in
  check string "part2 example3 is valid" expected actual
 
let suite =
  [
    "part1", `Quick, test_part1;
    (* "part2 example1", `Quick, test_part2_example1; *)
    (* "part2 example2", `Quick, test_part2_example2; *)
    (* "part2 example3", `Quick, test_part2_example3; *)
  ]



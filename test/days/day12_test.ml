open Alcotest

let day = new Advent_of_code_2023.Day12.t

let i = {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}

let test_part1 () =
  let expected = "21" in
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



open Alcotest

let day = new Advent_of_code_2023.Day15.t

let i = {|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7|}

let test_part1 () =
  let expected = "1320" in
  let actual = day#part1 i in
  check string "part1 is valid" expected actual


let test_part2 () =
  let expected = "145" in
  let actual = day#part2 i in
  check string "part2 is valid" expected actual

 
let suite =
  [
    "part1", `Quick, test_part1;
    "part2", `Quick, test_part2;
  ]



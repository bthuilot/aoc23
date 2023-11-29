open Advent_of_code_2023

let usage_msg = "aoc23 <day> [<additional days>] ..."

module SI = Set.Make(Int)

let days_to_run = ref SI.empty

let anon_fun arg =
  match int_of_string_opt arg with
  | None -> ()
  | Some day -> days_to_run := SI.add day !days_to_run


let () =
  Arg.parse [] anon_fun usage_msg;
  if
    SI.is_empty !days_to_run
  then
    days_to_run := List.init 25 ((+) 1) |> SI.of_list;
  
  print_endline "";
  print_endline "##########################";
  print_endline "## Advent of Code 2023  ##";
  print_endline "##########################";
  print_endline "";
  Calendar.days |> List.iteri (fun num d ->
                  if
                    SI.mem num !days_to_run
                  then (d#run; print_endline "")
                )
  
  

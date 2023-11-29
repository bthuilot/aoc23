open Advent_of_code_2023

let usage_msg = "aoc23 <day> [<additional days>] ..."

let days_to_run = ref []

let anon_fun filename = days_to_run := filename :: !days_to_run

let itoa = Printf.sprintf "%d"

let () =
  Arg.parse [] anon_fun usage_msg;
  
  if
    List.compare_length_with !days_to_run 0 = 0
  then
    days_to_run := List.init 25 (fun x -> itoa (x + 1));
  
  print_endline "";
  print_endline "##########################";
  print_endline "## Advent of Code 2023  ##";
  print_endline "##########################";
  print_endline "";
  Calendar.days |> List.iteri (fun num d ->
                  if
                    List.exists ((=) (itoa num)) !days_to_run
                  then (d#run; print_endline "")
                )
  
  

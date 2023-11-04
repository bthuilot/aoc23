open Advent_of_code_2023

let days_to_run = List.init 25 (fun x -> x + 1)
  

let () =
  print_endline "";
  print_endline "##########################";
  print_endline "## Advent of Code 2023  ##";
  print_endline "##########################";
  All.days |> List.iteri (fun num d ->
                  if List.exists (fun i -> i = num) days_to_run then d#run; print_endline ""
                )
  
  

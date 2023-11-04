open Advent_of_code_2023

let days_to_run = [0;] (*List.init 26 (fun x -> x)*)
  

let () =
  print_endline "";
  print_endline "##########################";
  print_endline "## Advent of Code 2023  ##";
  print_endline "##########################";
  print_endline "";
  days_to_run |> List.iter (fun num ->
                     let d = List.nth All.days num in
                     d#run
                   )
  
  

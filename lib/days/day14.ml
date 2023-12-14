open Utils

type dish = char list list

let parse_input (input : string) : dish =
  input
  |> String.split_on_char '\n' |> List.map (String.to_seq >> List.of_seq)

(* let find_total_load (dish : dish) : int= *)
(*   let rev_dish = List.rev dish in *)
(*   let falling = Array.make (List.length (List.hd dish)) 0 in *)
(*   let rec find_row_load (index: int) (row: char list) : int = *)
(*     match row with *)
(*     | [] -> 0 *)
(*     | x :: xs -> *)
(*        match x with *)
(*        | '  *)
(*       | '#' -> falling.(index) <- falling.(index) + 1; find_row_load (index + 1) xs *)
(*   in *)
(*   List.mapi (fun i row -> *)
(*       find_row_load i row *)
(*     ) rev_dish |> List.fold_left (+) 0 *)

let find_total_load (dish : dish) : int =
  let t_dish = transpose dish in
  let rec tilt_row (total: int) (rocks : int) (row: char list) : char list =
    let stack_rocks t r = List.init (t - r) (fun _ -> '.') @ List.init r (fun _ -> 'O') in
    match row with
    | [] -> stack_rocks total rocks
    | '#' :: xs -> (stack_rocks total rocks) @ '#' :: (tilt_row 0 0 xs)
    | c :: xs -> tilt_row (total + 1) (rocks + if c = 'O' then 1 else 0) xs
  in
  let titled = List.map (
                   fun r -> Printf.printf "%s\n" (String.of_seq (List.to_seq r));
                            let r = tilt_row 0 0 (List.rev r)
                            in
                            Printf.printf "%s\n" (String.of_seq (List.to_seq r));
                            print_endline "-----";
                            List.rev r) t_dish |> transpose
  in
  Printf.printf "%s\n" (List.map (List.to_seq >> String.of_seq) titled |> String.concat "\n");
  List.mapi (fun i r ->
      let rocks = List.filter (fun c -> c = 'O') r |> List.length
      in
      rocks * (List.length titled - i )
    ) titled |> List.fold_left (+) 0


class t =
  object (_)
    inherit Day_intf.t 14
    method part1 (i : string) : string =
      let reflector_dish = parse_input i in
      find_total_load reflector_dish |> string_of_int
    method part2 (_ : string) : string = "Not Implemented"
  end



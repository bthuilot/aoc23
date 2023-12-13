
type pattern = char list list

type mirrors = pattern list

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)

let parse_mirrors (i : string) : mirrors =
  let groups = Str.split (Str.regexp "\n\n") i in
  let parse_group (g : string) : pattern =
    let lines = Str.split (Str.regexp "\n") g in
    List.map (fun l -> List.init (String.length l) (String.get l)) lines
  in
  List.map parse_group groups

let find_row_mirror (p : pattern) : int option =
  let rec is_mirror (front: pattern) (back : pattern) : bool =
    match front, back with
    | [], _  -> true
    | _, []  -> true
    | f :: fs, b :: bs -> f = b && is_mirror fs bs
  in
  let rec find_row_mirror' (front : pattern) (back : pattern) (i : int) : int option =
    match front, back with
    | [], _  -> None
    | _, []  -> None
    | _, b :: bs ->
       if is_mirror front back then Some i
       else find_row_mirror' (b :: front) (bs) (i + 1) in
  find_row_mirror' [(List.hd p)] (List.tl p) 1

class t =
  object (_)
    inherit Day_intf.t 13
    method part1 (i : string) : string =
      let mirrors = parse_mirrors i in
      List.fold_left (fun acc m ->
          let value = match find_row_mirror m with
            | Some i -> i * 100
            | None -> match find_row_mirror (transpose m) with
                      | Some i -> i
                      | None -> 0
          in
          acc + value
        ) 0 mirrors |> string_of_int
    method part2 (_ : string) : string = "Not Implemented"
  end



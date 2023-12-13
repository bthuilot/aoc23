
type spring_row = char list

type condition_record = {
    row: spring_row;
    groups: int list;
  }

let parse_groups (i : string) : int list =
  let groups = String.split_on_char ',' i in
  List.map int_of_string groups
  

let parse_condition_records (i : string) : condition_record list =
  let rows = String.split_on_char '\n' i in
  let parse_line (l : string) : condition_record =
    let l_split = String.split_on_char ' ' l in
    match l_split with
    | [row; groups] ->
       let chars = String.to_seq row |> List.of_seq in
       {
         row = chars;
         groups = parse_groups groups;
       }
    | _ -> failwith "Invalid input"
  in
  List.map parse_line rows

let rec drop i l =
  if i = 0 then l else drop (i - 1) (List.tl l)

let rec take i l =
  if i = 0 then [] else (List.hd l) :: take (i - 1) (List.tl l)

let find_arrangements (r: condition_record) : int =
  let dyn = Array.make_matrix (List.length r.row + 1) (List.length r.groups + 1) (-1) in
  let rec f' (l: char list) (groups: int list) : int =
    if dyn.(List.length l).(List.length groups) >= 0 then
      dyn.(List.length l).(List.length groups)
    else
    match groups with
    | [] -> if (List.exists ((=) '#') l) then 0 else 1
    | g :: rst ->
       if g > List.length l then 0 else
         let selection = take g l in
         let remainder = drop g l in
         let (first, _) = (List.hd selection, (List.nth selection (g-1))) in
         let split_on_broken = List.length remainder > 0 && (List.hd remainder) = '#' in
         let should_mark_next_op = List.length remainder > 0 && (List.hd remainder) = '?' in
         let has_op = List.exists ((=) '.') selection  in
         let amt_from_point = if has_op || split_on_broken then 0 else
                                f' (if should_mark_next_op then List.tl remainder else remainder) rst in
         let amt_from_rec = if first = '#' then 0 else f' (List.tl l) groups in
         let res = amt_from_point + amt_from_rec in
         dyn.(List.length l).(List.length groups) <- res;
         res
  in
  let res = f' r.row r.groups
  in

  res



let flatten_with_separator sep lists =
  let rec aux acc = function
    | [] -> acc
    | [x] -> List.rev_append x acc (* No separator for the last sublist *)
    | x :: xs -> aux (sep :: List.rev_append x acc) xs
  in
  aux [] (List.rev lists)

let unfold_records (amt: int) (records: condition_record list) : condition_record list =
  (* Function to flatten a list of lists with a separator *)
  let rec flatten s = function
    | [] -> []
    | x0 :: x1 :: xs -> (x0 @ [s] @ flatten s (x1 :: xs))
    | [x] -> x
  in
  List.map (fun r -> {
                row = flatten '?' (List.init amt (fun _ -> r.row));
                groups = List.init amt (fun _ -> r.groups) |> List.concat;
    }) records

 
class t =
  object (_)
    inherit Day_intf.t 12
    method part1 (i : string) : string =
      let records = parse_condition_records i in
      List.fold_left (fun acc r -> acc + (find_arrangements r)) 0 records |> string_of_int
    method part2 (i : string) : string =
      let records = parse_condition_records i in
      let unfolded = unfold_records 5 records in
      List.fold_left (fun acc r ->
          acc + (find_arrangements r)
        ) 0 unfolded |> string_of_int
      
  end



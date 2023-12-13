

type spring =
  | Operational
  | Broken of int
  | Unknown of int

type spring_row = spring list

type condition_record = {
    row: spring_row;
    groups: int list;
  }

let parse_groups (i : string) : int list =
  let groups = String.split_on_char ',' i in
  List.map int_of_string groups
  

let parse_condition_records (i : string) : condition_record list =
  let rows = String.split_on_char '\n' i in
  let parse_spring (c : char) : spring =
    match c with
    | '.' -> Operational
    | '#' -> Broken 1
    | _ -> Unknown 1
  in
  let rec parse_row (acc : spring) (r : char list) : spring_row =
    match r with
    | [] -> [acc]
    | c :: rst ->
       match acc with
       | Operational ->
          if c = '.' then
            parse_row Operational rst
          else
            acc :: parse_row (parse_spring c) rst
       | Broken n ->
          if c = '#' then
            parse_row (Broken (n + 1)) rst
          else
            acc :: parse_row (parse_spring c) rst
       | Unknown n ->
          if c = '?' then
            parse_row (Unknown (n + 1)) rst
          else
            acc :: parse_row (parse_spring c) rst
  in
  let parse_line (l : string) : condition_record =
    let l_split = String.split_on_char ' ' l in
    match l_split with
    | [row; groups] ->
       let chars = String.to_seq row |> List.of_seq in
       {
         row = parse_row (parse_spring (List.hd chars)) (List.tl chars);
         groups = parse_groups groups;
       }
    | _ -> failwith "Invalid input"
  in
  List.map parse_line rows


let calculate_arrangements (r : condition_record) : int =
  let rec calculate_arrangement' (acc : int) (r : spring_row) : int =
    match r with
    | [] -> acc
    | Operational :: rst -> calculate_arrangement' acc rst
    | Broken n :: rst -> calculate_arrangement' (acc + n) rst
    | Unknown n :: rst -> calculate_arrangement' (acc + n) rst
  in
  calculate_arrangement' 0 r.row
  

class t =
  object (_)
    inherit Day_intf.t 12
    method part1 (i : string) : string =
      let records = parse_condition_records i in
      List.iter (fun r ->
          List.iter (fun s ->
              match s with
              | Operational -> print_string "| ."
              | Broken n -> print_string @@ "| Broken: " ^(string_of_int n)
              | Unknown n -> print_string @@ "| Unkown: " ^ (string_of_int n)
            ) r.row;
          print_string "| Groups: ";
          List.iter (fun g -> print_string @@ (string_of_int g) ^ ",") r.groups;
          print_newline ();
        ) records;
      ""
    method part2 (_ : string) : string = "Not Implemented"
  end



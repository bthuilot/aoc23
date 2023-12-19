type machine_part = {
    a: int;
    s: int;
    m: int;
    x: int;
  }

type state = | A | R | Workflow of string

type rating = X | S | M | A

type comparator = {
    rating: rating;
    f: (int -> bool);
    state: state;
  }
    

type rule = Comparision of comparator | State of state

type workflow = {
    name: string;
    rules: rule list;
  }

type workflows = (string, workflow) Hashtbl.t

type machine = {
    workflows: workflows;
    parts: machine_part list;
  }

let parse_state (s: string) : state =
  match s with
  | "A" -> A
  | "R" -> R
  | s -> Workflow s

let parse_rating (r: string) : rating =
  match r with
  | "x" -> X
  | "s" -> S
  | "m" -> M
  | "a" -> A
  | _ -> failwith "Invalid rating"

let parse_comp (c: string) : (int -> int -> bool) =
  match c with
  | "<" -> (>)
  | ">" -> (<)
  | _ -> failwith "Invalid comparator"

let parse_workflows (i: string) : (string, workflow) Hashtbl.t =
  let lines = String.split_on_char '\n' i in
  let tbl = Hashtbl.create (List.length lines) in
  let parse_rule (r: string) : rule =
    let is_single_state = Str.string_match (Str.regexp {|^\([a-zRA]+\)$|}) r 0 in
    if is_single_state then
      State (Str.matched_group 1 r |> parse_state)
    else
      let _ = Str.string_match (Str.regexp {|^\([amsx]\)\([<>]\)\([0-9]+\):\([a-zRA]+\)$|}) r 0 in
      let (rating, comp, amt, state) = Str.matched_group 1 r, Str.matched_group 2 r,  Str.matched_group 3 r, Str.matched_group 4 r in
      Comparision ({
          rating = parse_rating rating;
          f = (parse_comp comp) (int_of_string amt);
          state = parse_state state;
        })
  in
  let parse_workflow (w: string) =
    let _ = Str.string_match (Str.regexp {|\([a-z]+\){\([a-zAR><,:0-9]+\)}|}) w 0 in
    let name = Str.matched_group 1 w in
    let rules_strs = Str.matched_group 2 w |> String.split_on_char ',' in
    Hashtbl.add tbl name { name; rules = List.map parse_rule rules_strs };
  in
  List.iter parse_workflow lines;
  tbl

let parse_parts (ps: string) : machine_part list =
  let parse_part (i: string): machine_part =
    let _ = Str.string_match (Str.regexp {|{x=\([0-9]+\),m=\([0-9]+\),a=\([0-9]+\),s=\([0-9]+\)}|}) i 0 in
    let x = Str.matched_group 1 i |> int_of_string in
    let m = Str.matched_group 2 i |> int_of_string in
    let a = Str.matched_group 3 i |> int_of_string in
    let s = Str.matched_group 4 i |> int_of_string in
    { x; m; a; s }
  in
  String.split_on_char '\n' ps |> List.map parse_part
    
  

let parse_input (input : string) : machine =
  let split = Str.split (Str.regexp "\n\n") input in
  let workflows = parse_workflows (List.hd split) in
  let parts = parse_parts (List.nth split 1) in
  { workflows; parts }

let filter_accepted_parts (workflows: workflows) (parts: machine_part list) : machine_part list =
  let filter_part (p: machine_part) : bool =
    let rec filter_rules (rules: rule list) : bool =
      let transition (state: state): bool =
        match state with
        | A -> true
        | R -> false
        | Workflow w -> filter_rules (Hashtbl.find workflows w).rules;
      in
      match rules with
      | [] -> false (* this should never happen *)
      | State s :: _ -> transition s
      | Comparision c :: rst ->
         let value = match c.rating with
           | X -> p.x
           | S -> p.s
           | M -> p.m
           | A -> p.a
         in
         if c.f value then
           transition c.state
         else
           filter_rules rst
    in
    filter_rules (Hashtbl.find workflows "in").rules
  in
  List.filter filter_part parts
  


class t =
  object (_)
    inherit Day_intf.t 19
    method part1 (i : string) : string =
      let machine = parse_input i in
      let accepted_parts = filter_accepted_parts machine.workflows machine.parts in
      List.fold_left (fun acc p ->
          acc + (p.x + p.m + p.a + p.s)
        ) 0 accepted_parts |> string_of_int
    method part2 (_ : string) : string = "Not Implemented"
  end



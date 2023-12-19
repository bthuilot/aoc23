(** Day 19: Aplenty

    https://adventofcode.com/2023/day/19

    This one just made my wrists hurt. Whole lotta
    work spent soley on parsing the input. The idea behind it was
    relatively simple, just an FSA (thank you theory of computation),
    but it just took a lot of work to get there haha.

    For part 1 I parsed each workflow into a hash table
    mapping name to workflow. Each workflow is a list of
    rules. Each rule is either a state (i.e. move directly to that state)
    or a comparision, which moves to a state based on the value of a rating
    (x, s, m, a) and a comparator (<, >) and a value. It was then just a matter
    of parsing the parts, and for each, checking if it was accepted by the
    workflow (i.e. starting at in, and following the rules until you reach A or R).

    For part 2, I simply created a new record similar to the part, but held ranges
    instead of single values. Each time I came to a Comparision, I split into two
    recursive calls, one where the comparision passed and one where the comparision didnt.
    I would update the ranges according on each subcall. Every time I reached a 'A' node I
    would return the ranges. The function would aggregate all returned ranges into a
    list. This will give the # of combinations, by multiplying the spread of each range
    for each part, then adding them all together.
    
 *)

(** [machine_part] represents a part of the machine. It has 4 ratings, x, s, m, a, each
    of which is an integer.
 *)
type machine_part = {
    a: int;
    s: int;
    m: int;
    x: int;
  }

(** [state] represents a state in the machine. It is either A (accepted), R (rejected),
    or a workflow name.
 *)
type state = | A | R | Workflow of string

(** [rating] represents a rating of a part. It is either X, S, M, or A.
 *)
type rating = X | S | M | A

(** [compare_func] represents a function that compares a rating to a value. It is either
    LessThan of int or GreaterThan of int.
 *)
type compare_func = LessThan of int | GreaterThan of int

(** [comparator] represents a comparision of a rating to a value. It has a rating, a
    compare_func, and a state to move to if the comparision passes.
 *)
type comparator = {
    rating: rating;
    f: compare_func;
    state: state;
  }
    
(** [rule] represents a rule in a workflow.
    It is either a comparision or a state.
    A Comparision is a comparator, where it compares a rating to a value and
    moves to a state if the comparision passes. A State is a transition directly to a state.
 *)
type rule = Comparision of comparator | State of state

(** [workflow] represents a workflow in the machine.
    It has a name and a list of rules.
    The rules are applied in order, and the first rule that passes is the one that is used.
 *)
type workflow = {
    name: string;
    rules: rule list;
  }

(** [workflows] represents a hash table mapping workflow names to workflows.
 *)
type workflows = (string, workflow) Hashtbl.t

(** [machine] represents a machine.
    It has a list of workflows and a list of parts.
 *)
type machine = {
    workflows: workflows;
    parts: machine_part list;
  }

(** [parse_state s] parses a string into a state.
    It is either A, R, or a Workflow.
 *)
let parse_state (s: string) : state =
  match s with
  | "A" -> A
  | "R" -> R
  | s -> Workflow s

(** [parse_rating r] parses a string into a rating.
    It is either X, S, M, or A.
 *)
let parse_rating (r: string) : rating =
  match r with
  | "x" -> X
  | "s" -> S
  | "m" -> M
  | "a" -> A
  | _ -> failwith "Invalid rating"

(** [perform_comp c a] performs a comparision on a rating and a value.
    It is either LessThan or GreaterThan.
 *)
let perform_comp (c: compare_func) (a: int) : bool =
  match c with
  | LessThan i -> a < i
  | GreaterThan i -> a > i

(** [parse_workflows i] parses a string into a hash table of workflows.
    It creates a hash table mapping workflow to its name.
    The string is a list of workflows seperated by newlines.
    Each workflow should be of the form:
    N\{R1, R2, R3, ...\}
    where N is the name of the workflow, and R1, R2, R3, ... are the rules.
 *)
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
            state = parse_state state;
            f = match comp with
                | "<" -> LessThan (int_of_string amt);
                | ">" -> GreaterThan (int_of_string amt);
                | _ -> failwith "Invalid comparator";
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

(** [parse_parts ps] parses a string into a list of machine parts.
    It iterates over the lines, and parses each line into a machine part.
    each line is of the form {x=N,m=N,a=N,s=N}, where N is an integer.
 *)
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
    
(** [parse_input input] parses a string into a machine.
    It parses the string into two parts, separated by two newlines.
    The first part is the workflows parsed with [parse_workflows],
    and the second part is the parts parsed with [parse_parts].
 *)
let parse_input (input : string) : machine =
  let split = Str.split (Str.regexp "\n\n") input in
  let workflows = parse_workflows (List.hd split) in
  let parts = parse_parts (List.nth split 1) in
  { workflows; parts }


(** [filter_accepted_parts workflows parts] filters the list of parts
    to only those that are accepted by the workflows.
 *)
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
         if perform_comp c.f value then
           transition c.state
         else
           filter_rules rst
    in
    filter_rules (Hashtbl.find workflows "in").rules
  in
  List.filter filter_part parts

(** [ranges] is a record of the ranges of each rating.
    Each rating is a tuple of (min, max).
 *)
type ranges = {
    x: (int * int);
    s: (int * int);
    m: (int * int);
    a: (int * int);
  }

(** [assign_new_range rs rating r] assigns the range [r] to the rating [rating]
    in the ranges [rs].
 *)
let assign_new_range (rs: ranges) (rating: rating) (r: int * int) : ranges =
  match rating with
  | X -> { rs with x = r }
  | S -> { rs with s = r }
  | M -> { rs with m = r }
  | A -> { rs with a = r }

(** [find_combinations workflows] finds the number of combinations of parts
    that are accepted by the workflows.
 *)
let find_combinations (workflows: workflows): int =
  let init = { x = (1, 4000); s = (1, 4000); m = (1, 4000); a = (1, 4000) } in
  let rec find_ranges (r: ranges) (rules: rule list) : ranges list  =
    let transition (r': ranges) (state: state): ranges list =
      match state with
      | A -> [r']
      | R -> []
      | Workflow w -> find_ranges r' (Hashtbl.find workflows w).rules;
    in
    match rules with
    | [] -> [] (* this should never happen *)
    | State s :: _ -> transition r s
    | Comparision c :: rst ->
       let value = match c.rating with
         | X -> r.x
         | S -> r.s
         | M -> r.m
         | A -> r.a
       in
       let (lower, upper) = value in
       let new_pass_range = match c.f with
         | LessThan i ->
            if i < lower then None else
            Some (lower, min (i - 1) upper)
         | GreaterThan i ->
            if i > upper then None else
              Some (max (i + 1) lower, upper)
       in
       let new_fail_range = match c.f with
         | LessThan i ->
            if i > upper then None else
              Some (max i lower, upper)
         | GreaterThan i ->
            if i < lower then None else
              Some (lower, min i upper)
       in
       let passing = if Option.is_none new_pass_range then [] else
                       let new_rs = assign_new_range r c.rating (Option.get new_pass_range) in
                       transition new_rs c.state
       in
       let failing = if Option.is_none new_fail_range then [] else
                       let new_rs = assign_new_range r c.rating (Option.get new_fail_range) in
                       find_ranges new_rs rst
       in
       passing @ failing
  in
  let rs = find_ranges init (Hashtbl.find workflows "in").rules in
  List.map (fun r ->
      (snd r.x - fst r.x + 1)
      * (snd r.s - fst r.s + 1)
      * (snd r.m - fst r.m + 1)
      * (snd r.a - fst r.a + 1)
    ) rs |> List.fold_left (+) 0
  
  
class t =
  object (_)
    inherit Day_intf.t 19
    
    method part1 (i : string) : string =
      let machine = parse_input i in
      let accepted_parts = filter_accepted_parts machine.workflows machine.parts in
      List.fold_left (fun (acc: int) (p: machine_part)  ->
          acc + (p.x + p.m + p.a + p.s)
        ) 0 accepted_parts |> string_of_int
    
    method part2 (i : string) : string =
      let machine = parse_input i in
      let combinations = find_combinations machine.workflows in
      string_of_int combinations
  end



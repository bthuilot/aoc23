(** Day 15:  Lens Library

    I'm seeing a pattern this year of challenging problems
    followed by an easy one the next day. Today's was easy.

    The first part was a simple foldl over a list of strings,
    "hashing" each string and adding it to the accumulator.

    The second part was just instead parsing each string into
    an "operation", either updating a value in an array or
    removing it. Then, we just iterate over the array and
    sum up the "focus power" of each value in the array.

    Honestly the hardest part was parsing the operations.
 *)

open Utils


(** [op] is an operation on a box value. It is either an insertion
    of a label and its focus power, or a removal of a label.
 *)
type op = Remove of string | Insert of (string * int)

(** [box_val] is a list of (string * int) pairs, where the string
    is the label of the value and the int is the value itself.
 *)
type box_val = (string * int) list

(** [parse_input input] parses the input into a list of strings,
    seperating on commas.
 *)
let parse_input (input : string) : string list=
  input |> String.split_on_char ','

(** [parse_ops input] parses the input into a list of operations,
    where each operation is either an insertion or a removal.
 *)
let parse_ops (input : string list) : op list =
  let rec parse_op (label : char list) (remain : char list) : op =
    match remain with
    | '=' :: xs -> Insert (String.of_seq (List.rev label |> List.to_seq), int_of_string (String.of_seq (xs |> List.to_seq)))
    | [ '-' ] -> Remove (String.of_seq (List.rev label |> List.to_seq))
    | c :: xs -> parse_op (c :: label) xs
    | _ -> failwith "Invalid input"
  in
  List.map (String.to_seq >> List.of_seq >> parse_op []) input

(** [hash starting_val c] is a "hash" (as per the problem description)
    of the character [c] with the starting value [starting_val].
 *)
let hash (starting_val: int) (c: char) : int =
  ((starting_val + (Char.code c)) * 17) mod 256

(** [hash_str starting_val str] computes the [hash] of each character
    in [str] with the starting value [starting_val].
 *)
let hash_str (starting_val: int) (str : string) : int =
  String.fold_left hash starting_val str

(** [create_hash_box ops] creates a hash box from the list of operations
    [ops]. A hash box is an array of box values, where each box value
    is a list of (string * int) pairs. The string is the label of the
    value and the int is the value itself.
 *)
let create_hash_box (ops : op list) : box_val array =
  (* updates a label in a box value, or inserts it if it doesn't exist *)
  let rec update_or_insert (box_val : box_val) (s : string) (i : int) : box_val =
      match box_val with
      | [] -> [(s, i)]
      | (s', i') :: xs ->
         if s = s' then (s, i) :: xs
         else (s', i') :: update_or_insert xs s i
  in let rec insert (ops : op list) (arr : box_val array): box_val array =
    match ops with
    | [] -> arr
    | Insert (s, i) :: xs ->
       let hash = hash_str 0 s in
       arr.(hash) <- update_or_insert arr.(hash) s i;
       insert xs arr
    | Remove s :: xs ->
       let hash = hash_str 0 s in
       (* filter list of (string * int) pairs to remove the pair with label [s] *)
       arr.(hash) <- (List.filter (fst >> ((<>) s)) arr.(hash));
       insert xs arr
  in Array.make 256 [] |> insert ops

(** [calc_focusing_power box i acc] calculates the sum focusing powers of
    [box] from index [i] to the end of the array, adding the result to
    accumulator [acc]. The focusing power of a box value is the sum
    of the focusing power of each value in the box value. The focusing
    power of a value is the product of the value's row index plus one, the
    value's col index plus 1, and the value itself.
 *)
let rec calc_focusing_power (box: box_val array) (i: int) (acc: int)  : int =
  let rec find_row_power (j : int) (acc' : int) (box_val : box_val) : int =
    let focus_power len = (i + 1) * (j + 1) * len in
    match box_val with
    | [] -> acc'
    | (_, focal) :: xs -> find_row_power (j + 1) (acc' + focus_power focal) xs
  in
  if i > Array.length box - 1 then acc
  else let row = find_row_power 0 0 box.(i) in
       calc_focusing_power box (i + 1) (acc + row)

class t =
  object (_)
    inherit Day_intf.t 15
    method part1 (i : string) : string =
      let init_seq = parse_input i in
      List.fold_left (fun acc s ->
          acc + (hash_str 0 s)
        ) 0 init_seq |> string_of_int
      
    method part2 (i : string) : string =
      let init_seq = parse_input i in
      let ops = parse_ops init_seq in
      let box = create_hash_box ops in
      calc_focusing_power box 0 0 |> string_of_int
  end



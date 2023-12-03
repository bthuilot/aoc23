(** Day 3: Gear Ratios
    https://adventofcode.com/2019/day/3

    This problem is a bit of a mess as im still trying to get used to OCaml.
    My initial solutions were very imperative, so my final one uses a bit more
    of the OCaml type system and is a bit more functional. While I believe I
    could optimize this solution a bit more, I think it trades off readability.

    I again took the approach of parsing the input into a data structure that
    is easier to work with. I used a hashtable to store the symbols with the key
    being their [location]s, and another hashtable to store the numbers and their [num_location]s.
    I then used these data structures to find the parts and gear ratios.

    For the parts, I iterated through the numbers hashtable and checked if any
    of the [num_location]s were adjacent to a symbol. If they were, I added the
    number to a list of parts.

    For the gear ratios, I iterated through the numbers hashtable and checked
    if any of the [num_location]s were adjacent to a '*' symbol. If they were, I
    add the number to a set of gears to a new hash map, where the key is the
    location of the specific '*' symbol. I then iterated through the hash map of
    all unique '*' symbol locations and for each set of gears, I multiplied them
    together (if the set was length 2) and then finally added them to all together.
 *)


(** [SI] is a set of integers. *)
module SI = Set.Make(Int)

(** [is_digit c] is true if [c] is a digit. *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

(** [is_symbol c] is true if [c] is a symbol.
    Symbols are any character that is not a digit or a '.'.
 *)
let is_symbol = function
  | '0' .. '9' | '.' -> false
  | _ -> true

(** [location] is a tuple of the row and column of a symbol. *)
type location = (int * int)

(** [engine_symbols] is a hashtable of symbols and their locations. *)
type engine_symbols = (location, char) Hashtbl.t


(** [num_location] is a tuple of the start and end column of a number and the
    row it is on. *)
type num_location = {
    start_col: int;
    end_col: int;
    row: int
  }

(** [engine_numbers] is a hashtable of numbers and their locations. *)
type engine_numbers = (int, num_location) Hashtbl.t


(** [parse_engine_symbols l] will parse the list of strings [l] into a hashtable
    of symbols and their locations.
 *)
let parse_engine_symbols (l : string list) : engine_symbols =
  let h = Hashtbl.create (10 * List.length l) in
  List.iteri (fun row s ->
      String.iteri (fun col c ->
          if is_symbol c then
          Hashtbl.add h (row, col) c
        ) s
    ) l;
  h

(** [bordering_cords row col] will return a list of [location]s that are
    bordering [(row, col)]. *)
let bordering_cords (row : int) (col : int) : location list =
  [(row - 1, col - 1); (row - 1, col); (row - 1, col + 1);
   (row, col - 1); (row, col + 1);
   (row + 1, col - 1); (row + 1, col); (row + 1, col + 1)]

(** [is_symbol_adjancet symbls row col] is true if the symbol at [(row, col)] is
    adjacent to any symbol in [symbls].
 *)
let is_symbol_adjancet (symbls : engine_symbols) (row : int) (col : int) : bool =
  bordering_cords row col
  |> List.exists (fun (row, col) -> Hashtbl.mem symbls (row, col))

type parse_stack = {curr_num: char list; start_col: int; end_col: int}
type parse_engine_num_acc = {stack: parse_stack; col: int}

(** [char_list_to_int l] will convert the list of characters [l] into an int. *)
let char_list_to_int (l : char list) : int =
  if List.length l = 0 then 0
  else
    l |> List.rev |> List.to_seq |> String.of_seq |> int_of_string

(** [parse_engine_num_char h row acc c] is a helper function for [parse_engine_num_row].
    It is used to parse a single character [c] and add it to the accumulator [acc] if its a digit.
    If [c] is not a digit, it will add the number in the accumulator to the hashtable [h] (if there is one).
 *)
let parse_engine_num_char (h : engine_numbers) (row: int) (acc: parse_engine_num_acc) (c : char) : parse_engine_num_acc =
  if is_digit c then
    {
      stack = {
        curr_num = c :: acc.stack.curr_num;
        start_col = if List.is_empty acc.stack.curr_num then acc.col else acc.stack.start_col;
        end_col = acc.col
      };
      col = acc.col + 1;
    }
  else
    if List.length acc.stack.curr_num > 0 then
      let num = char_list_to_int acc.stack.curr_num in
      Hashtbl.add h num {
          start_col = acc.stack.start_col;
          end_col = acc.stack.end_col;
          row = row
        };
      {
        stack = {
          curr_num = [];
          start_col = 0;
          end_col = 0;
        };
        col = acc.col + 1;
      }
    else
      {acc with col = acc.col + 1}

(** [parse_engine_num_row h row s] is a helper function for [parse_engine_numbers].
    It is used to parse a single row [s] and add any numbers in it to the hashtable [h].
 *)
let parse_engine_num_row (h : engine_numbers) (row : int) (s : string) =
  let init_acc = {
      col = 0;
      stack = {curr_num = []; start_col = 0; end_col = 0}
    } in
  let final_acc =
    String.fold_left
      (parse_engine_num_char h row) 
      init_acc
      s 
  in
  if List.length final_acc.stack.curr_num > 0 then
    let num = char_list_to_int final_acc.stack.curr_num in
    Hashtbl.add h num {
        start_col = final_acc.stack.start_col;
        end_col = final_acc.stack.end_col;
        row = row
      }

(** [parse_engine_numbers l] will parse the list of strings [l] into a hashtable
    of numbers and their locations.
 *)
let parse_engine_numbers (l : string list) : engine_numbers =
  let h = Hashtbl.create (10 * List.length l) in
  List.iteri (parse_engine_num_row h) l;
  h

(** [find_parts s n] will return a list of numbers that are parts of the engine.
    A number is a part of the engine if it is adjacent to a symbol.
 *)
let find_parts (s : engine_symbols) (n : engine_numbers) : int list =
  Hashtbl.fold (fun num loc acc ->
      let {start_col; end_col; row} = loc in
      let cords = List.init (end_col - start_col + 1) (fun i -> (row, i + start_col)) in
      let is_part = List.exists (fun (row, col) ->
                bordering_cords row col
                |> List.exists (fun (row, col) -> Hashtbl.mem s (row, col))
                ) cords in
      if is_part then
        num :: acc
      else
        acc
    ) n []

(** [parse_gear_ratio_vals gears s num num_loc] is a helper function for [parse_gear_ratio].
    For a number [num] at location [num_loc] in the engine, it will determin which '*' symbols
    are adjacent to it using [s], and add the [num] to the hashtable [gears] to all keys that
    coorespond to the adjacent '*' symbols.
 *)
let parse_gear_ratio_vals (gears : (location, SI.t) Hashtbl.t) (s : engine_symbols) (num : int) (num_loc : num_location) =
  let {start_col; end_col; row} = num_loc in
  let cords = List.init (end_col - start_col + 1) (fun i -> (row, i + start_col)) in
  List.iter (fun (row, col) ->
      bordering_cords row col
      |> List.iter (fun (aRow, aCol) ->
             match Hashtbl.find_opt s (aRow, aCol) with
             | Some '*' ->
                let gear = Hashtbl.find_opt gears (aRow, aCol) in
                begin
                  match gear with
                  | Some g -> Hashtbl.replace gears (aRow, aCol) (SI.add num g);
                  | None -> Hashtbl.add gears (aRow, aCol) (SI.singleton num);
                end
             | _ -> ()
           )
    ) cords

(** [find_gear_ratios s n] will return a list of numbers that are gear ratios.
    A number is a gear ratio if it is adjacent to a gear.
 *)
let find_gear_ratios (s : engine_symbols) (n : engine_numbers) : int list =
  let gears = Hashtbl.create (Hashtbl.length n) in
  Hashtbl.iter (parse_gear_ratio_vals gears s) n;
  Hashtbl.fold (fun _ gears acc ->
      if SI.elements gears |> List.length == 2 then
        let product = SI.fold (fun a b -> a * b) gears 1 in
        product :: acc
      else
        acc
    ) gears []


class t =
  object (_)
    inherit Day_intf.t 3

    (* Part 1 *)
    method part1 (i : string) : string =
      let lines = String.split_on_char '\n' i in
      let symbols = parse_engine_symbols lines in
      let engine_numbers = parse_engine_numbers lines in
      let parts = find_parts symbols engine_numbers in
      parts |> List.fold_left (+) 0 |> string_of_int
    
    (* Part 2 *)
    method part2 (i : string) : string =
      let lines = String.split_on_char '\n' i in
      let symbols = parse_engine_symbols lines in
      let engine_numbers = parse_engine_numbers lines in
      let parts = find_gear_ratios symbols engine_numbers in
      parts |> List.fold_left (+) 0 |> string_of_int
  end



open Utils

(** Day 9: Mirage Maintenance

    https://adventofcode.com/2023/day/9

    This one was really easy, they practically give you the answer in
    the problem description. I'm not sure if there's a more efficient
    way to do it, but I just implemented the algorithm they describe.

    I pretty much fold through the list to compute the differences
    between each pair of adjacent numbers, then fold through that list
    to see if they are all zero. If they are, then I'm done, and return the
    last value of the original list. If they aren't, I recursively call
    the function on the list of differences, and add the result to the
    last value of the original list.
 *)

(** [history] is a list of numbers representing the history of the
    mirage's power levels.
 *)
type history = int list

(** [oasis] is a list of [history]s representing the mirage's power
    levels over time.
 *)
type oasis = history list


(** [parse_oasis input] parses [input] into an [oasis].
    Each line of [input] is a [history], and each number in a line is
    a power level.
 *)
let parse_oasis (input : string) : oasis =
  input
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ' ' >> List.map int_of_string)

(** [compute_row_diff h] computes the differences between each pair of
    adjacent numbers in [h], and returns the list of differences and
    the last value of [h].
 *)
let compute_row_diff (h : history) : history * int =
  let (last_val, next_row_rev) = List.fold_left (fun (prev, acc) cur -> (cur, cur - prev :: acc)) (List.hd h, []) (List.tl h) in
  List.rev next_row_rev, last_val

(** [find_next_val h] computes the next value in the sequence of
    numbers in [h] according to the problem description of part 1.

    It does this by computing the differences between each pair of
    adjacent numbers in [h], and then recursively calling itself on
    the list of differences. If all the differences are zero, then it
    returns the last value of [h]. Otherwise, it returns the last
    value of [h] plus the result of the recursive call.
 *)
let rec find_next_val (h : history) : int =
  let row_diff, last_val = compute_row_diff h in
  let all_zero = List.for_all ((=) 0) row_diff in
  if all_zero then
        last_val
  else
    let next_last_val = find_next_val row_diff
    in last_val + next_last_val

(** [find_prev_val h] computes the previous value in the sequence of
    numbers in [h] according to the problem description of part 2.

    It does this by computing the differences between each pair of
    adjacent numbers in [h], and then recursively calling itself on
    the list of differences. If all the differences are zero, then it
    returns the first value of [h]. Otherwise, it returns the first
    value of [h] minus the result of the recursive call.
 *)
let rec find_prev_val (h : history) : int =
  let row_diff, _ = compute_row_diff h in
  let all_zero = List.for_all ((=) 0) row_diff in
  if all_zero then
    List.hd h
  else
    let next_prev_val = find_prev_val row_diff
    in List.hd h - next_prev_val
  
  
        

class t =
  object (_)
    inherit Day_intf.t 9
    
    method part1 (i : string) : string =
      let oasis = parse_oasis i in
      List.map find_next_val oasis |> List.fold_left (+) 0 |> string_of_int
    
    method part2 (i : string) : string =
      let oasis = parse_oasis i in
      List.map find_prev_val oasis |> List.fold_left (+) 0 |> string_of_int    
  end



(** Day 1:  Trebuchet?!
    
    https://adventofcode.com/2023/day/1

    This day requires parsing 2 numbers from a each string in list, concatenating them,
    and summing them all together.

    The approach I took was to write a function that iterate through each string in the list,
    calling a generic function to parse the number at the current point in the string (or None if no number is found),
    and then concatenating the first and last number found. This meant parts 1 and 2 were almost identical,
    except for the function used to parse the number.

    For the acutal parsing functions, I decided to use pattern matching to do most of the work.
 *)

(** [is_digit c] is true if [c] is a digit. *)
let is_digit = function '0' .. '9' -> true  | _ -> false

(** [parse_digits s] will parse the first number it finds in [s].
    A number is a single digit 0-9.
 *)
let parse_digits = function
  | c :: _ when is_digit c -> Some (Char.code c - Char.code '0')
  | _ -> None

(** [parse_digits_and_words s] will parse the first number it finds in [s].
    A number is either a single digit or a word for a number.
 *)
let parse_digits_and_words = function
  | c :: _ when is_digit c -> Some (Char.code c - Char.code '0')
  | 'z' :: 'e':: 'r':: 'o' :: _ -> Some 0
  | 'o' :: 'n':: 'e' :: _ -> Some 1
  | 't' :: 'w':: 'o' :: _ -> Some 2
  | 't' :: 'h':: 'r' :: 'e' :: 'e' :: _ -> Some 3
  | 'f' :: 'o':: 'u' :: 'r' :: _ -> Some 4
  | 'f' :: 'i':: 'v' :: 'e' :: _ -> Some 5
  | 's' :: 'i':: 'x' :: _ -> Some 6
  | 's' :: 'e':: 'v' :: 'e' :: 'n' :: _ -> Some 7
  | 'e' :: 'i':: 'g' :: 'h' :: 't' :: _ -> Some 8
  | 'n' :: 'i':: 'n' :: 'e' :: _ -> Some 9
  | _ -> None

(** [calibration_value parse_func s] will parse a string for its "calibration value".
    The calibration value is the first number found in the string and the last number found in the string, concatenated.
    If there is only one number, it is returned twice.
    If there are no numbers, 0 is returned.
    [parse_func] is the function used to parse a string at a given position for the first number. It should return [None] if no number is found.
 *)
let calibration_value (parse_func : char list -> int option) (s : string) =
  let fst = ref Option.None in
  let lst = ref Option.None in
  let rec loop s =
        match s with
        | [] -> ()
        | _ :: cs ->
           match parse_func s with
           | None -> loop cs
           | Some i ->
              match !fst with
              | None -> fst := Some i; loop cs
              | Some _ -> lst := Some i; loop cs
  in
  loop (List.of_seq (String.to_seq s));
  match (!fst, !lst) with
  | Some a, Some b -> (a * 10) + b
  | Some a, None ->  (a * 10) + a
  | _ -> 0
  

(** [part1_calibrator] is a function that will parse a string for its calibration value using [parse_digits]. *)
let part1_calibrator = calibration_value parse_digits

(** [part2_calibrator] is a function that will parse a string for its calibration value using [parse_digits_and_words]. *)
let part2_calibrator = calibration_value parse_digits_and_words

(** [calibration_sum calibrator lines] is the sum of the calibration values of all the lines in [lines].
    [calibrator] is the function used to parse a string for its calibration value.
 *)
let calibration_sum f lines =
  let rec loop acc = function
    | [] -> acc
    | l :: ls -> loop (acc + f l) ls
  in
  loop 0 lines

class t =
  object (_)
    inherit Day_intf.t 1

    method part1 (input : string) : string =
      let lines = String.split_on_char '\n' input in
        calibration_sum part1_calibrator lines |> string_of_int

    method part2 (input : string) : string =
      let lines = String.split_on_char '\n' input in
        calibration_sum part2_calibrator lines |> string_of_int
  end

(** Day 6: Wait For It

    https://adventofcode.com/2023/day/6

    This one was a little too easy for day 6.
    Parsing the input was pretty simple, just parse
    to lists of ints and then zip them together. or in
    the second parts case, drop all non-numeric characters
    and then parse the int.

    Both the first and second parts rely on the same
    algorithm, only changing how the input is interpreted.

    The solution for me consited of search every addend for
    a number that would result in a product greater than the
    distance. I only needed to search the first half of addends
    (because the second half would be the same as the first half,
    just in reverse order) and only search till I found the pair
    that created the greater result, since all other pairs after
    would also be greater. This is a O(n) solution, but after talking
    with Rahul, he helped me realize I should have done a binary search
    resulting in O(log n). Maybe one day I'll go back and do that, but
    for now I'm saving my strength for the harder problems.
    
 *)


(** [race] is representation of a toy boat race.
    [time] is the duration of the race in seconds
    [distance] is the distance of the race in meters
 *)
type race = {
    time: int;
    distance: int;
  }

(** [races] is a list of [race]s *)
type races = race list

(** [parse_int_list s] returns a list of integers from a string [s] with
    whitespace as the delimiter
 *)
let parse_int_list s =
  Str.split (Str.regexp "[ ]+") (String.trim s) |> List.map int_of_string

(** [parse_full_int s] returns the integer value of a string [s] with all
    non-numeric characters removed
 *)
let parse_full_int s =
  Str.global_replace (Str.regexp "[^0-9]") "" s |> int_of_string

(** [pair_times_and_distances] will zip a two lists of ints together
    into a [races].
 *)
let pair_times_and_distances (times : int list) (distances : int list) : races =
  List.map2 (fun t d -> {time = t; distance = d}) times distances

(** [parse_race_document1] will parse the race document according to the
    first part of the problem. It will parse the two lists of ints and
    zip then together into a [races].
 *)
let parse_race_document1 (input : string) : races =
  let re = Str.regexp "Time: \\([0-9 ]+\\)\nDistance: \\([0-9 ]+\\)" in
  let _ = Str.string_match re input 0 in
  let times = Str.matched_group 1 input in
  let distances = Str.matched_group 2 input in
  let time_list = parse_int_list times in
  let distance_list = parse_int_list distances in
  pair_times_and_distances time_list distance_list

(** [parse_race_document2] will parse the race document according to the
    second part of the problem. It will parse the two lists of ints by
    removing all non-numeric characters, so that they parsed rather
    as just two integers.
 *)
let parse_race_document2 (input : string) : race =
  let re = Str.regexp "Time: \\([0-9 ]+\\)\nDistance: \\([0-9 ]+\\)" in
  let _ = Str.string_match re input 0 in
  let times = Str.matched_group 1 input in
  let distances = Str.matched_group 2 input in
  let time = parse_full_int times in
  let distance = parse_full_int distances in
  {time = time; distance = distance}


(** [count_possible_wins r] returns the number of possible wins for a race [r]
    TODO: this can be done with a binary search, but I'm lazy lol
 *)
let count_possible_wins (r: race) : int =
  let time = r.time in
  let distance = r.distance in
  let rec find_possible_wins' (hold : int) (travel : int) : int = begin
    if hold > travel then
      0
    else
      if hold * travel > distance then (1 + time) - (2 * hold)
      else
        find_possible_wins' (hold + 1) (travel - 1);
    end in
  find_possible_wins' 0 time

class t =  
  object (_)
    inherit Day_intf.t 6
    
    method part1 (i : string) : string =
      let rs = parse_race_document1 i in
      List.map count_possible_wins rs |> List.fold_left (fun a w -> a * w) 1
      |> string_of_int

    
    method part2 (i: string) : string =
      let r =  parse_race_document2 i in
      count_possible_wins r |> string_of_int
      
  end



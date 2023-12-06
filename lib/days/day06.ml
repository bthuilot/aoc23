type race = {
    time: int;
    distance: int;
  }

type races = race list

let parse_int_list s =
  Str.split (Str.regexp "[ ]+") (String.trim s) |> List.map int_of_string

let parse_times_and_distances (times : int list) (distances : int list) : races =
  List.map2 (fun t d -> {time = t; distance = d}) times distances

let parse_race_document1 (input : string) : races =
  let re = Str.regexp "Time: \\([0-9 ]+\\)\nDistance: \\([0-9 ]+\\)" in
  let _ = Str.string_match re input 0 in
  let times = Str.matched_group 1 input in
  let distances = Str.matched_group 2 input in
  let time_list = parse_int_list times in
  let distance_list = parse_int_list distances in
  parse_times_and_distances time_list distance_list

let parse_full_int s =
  Str.global_replace (Str.regexp "[^0-9]") "" s |> int_of_string

let parse_race_document2 (input : string) : race =
  let re = Str.regexp "Time: \\([0-9 ]+\\)\nDistance: \\([0-9 ]+\\)" in
  let _ = Str.string_match re input 0 in
  let times = Str.matched_group 1 input in
  let distances = Str.matched_group 2 input in
  let time = parse_full_int times in
  let distance = parse_full_int distances in
  {time = time; distance = distance}


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



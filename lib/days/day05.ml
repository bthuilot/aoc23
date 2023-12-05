(** Day 5: If You Give A Seed A Fertilizer

    https://adventofcode.com/2023/day/5

    This days was easy to understand, but a bit tedious to implement.
    I first took the approach of parsing the input into a Hash table
    mapping each value to its mapped value for each of the _ to _ maps,
    leaving out the values that mapped to themselves. Test passed but
    my program hung so i decided to instead just parse the input into
    a record contianing the source and destanation starts and the range.
    Then when i needed to calculate the value for a key, i just iterated
    over the list of ranges until i found the one that contained the key
    and then calculated the value from the source and destanation starts.
    If I didn't find a range that contained the key, i just returned the
    key.

    the first part just required me to find the lowest location value calculated 
    for each seed in the seed list, so the refactor ran pretty quickly.

    The second part still required the lowest location value, but you now
    use the seed list to generate an isanely large list of seeds. My solution
    is *NOT* fast, but I let it run anyways and in about ~10 minutes it
    gave the correct answer. I'm sure there is a better way to do this, but
    I'm not sure what it is.
 *)


(** [almanac_range] is a record containing the start of the source range,
    the start of the destination range, and the range size. This is used
    to represent an individual mapped range for each _to_ map in the almanac.
 *)
type almanac_range = {
    dest_start : int;
    source_start : int;
    range : int;
  }

(** [almanac_map] is a list of [almanac_range]s. This is used to represent
    the mapped ranges for each _to_ map in the almanac.
 *)
type almanac_map = almanac_range list

(** [almanac] is a record containing the seed list and all the _to_ maps
    in the almanac.
 *)
type almanac = {
    seeds : int list;
    seed_to_soil : almanac_map;
    soil_to_fertilizer : almanac_map;
    fertilizer_to_water : almanac_map;
    water_to_light : almanac_map;
    light_to_temperature : almanac_map;
    temperature_to_humidity : almanac_map;
    humidity_to_location : almanac_map;
  }

(* Regexes for parsing the input *)

(** [seeds_re_str] is the regex string for parsing the seed list. *)
let seeds_re_str = "seeds: \\([0-9 ]+\\)"

(** [seed_to_soil_re_str] is the regex string for parsing the seed-to-soil map. *)
let seed_to_soil_re_str = "seed-to-soil map:\n\\([0-9 \n]+\\)"

(** [soil_to_fertilizer_re_str] is the regex string for parsing the soil-to-fertilizer map. *)
let soil_to_fertilizer_re_str = "soil-to-fertilizer map:\n\\([0-9 \n]+\\)"

(** [fertilizer_to_water_re_str] is the regex string for parsing the fertilizer-to-water map. *)
let fertilizer_to_water_re_str = "fertilizer-to-water map:\n\\([0-9 \n]+\\)"

(** [water_to_light_re_str] is the regex string for parsing the water-to-light map. *)
let water_to_light_re_str = "water-to-light map:\n\\([0-9 \n]+\\)"

(** [light_to_temperature_re_str] is the regex string for parsing the light-to-temperature map. *)
let light_to_temperature_re_str = "light-to-temperature map:\n\\([0-9 \n]+\\)"

(** [temperature_to_humidity_re_str] is the regex string for parsing the temperature-to-humidity map. *)
let temperature_to_humidity_re_str = "temperature-to-humidity map:\n\\([0-9 \n]+\\)"

(** [humidity_to_location_re_str] is the regex string for parsing the humidity-to-location map. *)
let humidity_to_location_re_str = "humidity-to-location map:\n\\([0-9 \n]+\\)"

(** [almanac_re] is the regex string for parsing the entire almanac. *)
let almanac_re = Str.regexp
                   (String.concat "\n\n" [
                        seeds_re_str;
                        seed_to_soil_re_str;
                        soil_to_fertilizer_re_str;
                        fertilizer_to_water_re_str;
                        water_to_light_re_str;
                        light_to_temperature_re_str;
                        temperature_to_humidity_re_str;
                        humidity_to_location_re_str
                   ])

(** [parse_map input] parses the input string into an [almanac_map]. *)
let parse_map (input : string) : almanac_map =
  let lines = Str.split (Str.regexp "\n") input in
  List.map (fun line ->
              let parts = Str.split (Str.regexp " ") line in
              let value = int_of_string (List.hd parts) in
              let key = int_of_string (List.nth parts 1) in
              let range = int_of_string (List.nth parts 2) in
              {
                dest_start = value;
                source_start = key;
                range = range;
              }
    ) lines


(** [parse_almanac input] parses the input string into an [almanac]. *)
let parse_almanac (input : string) : almanac =
  let _ = Str.string_match almanac_re input 0 in
  let seeds_i = Str.matched_group 1 input in
  let seed_to_soil_i = Str.matched_group 2 input in
  let soil_to_fertilizer_i = Str.matched_group 3 input in
  let fertilizer_to_water_i = Str.matched_group 4 input in
  let water_to_light_i = Str.matched_group 5 input in
  let light_to_temperature_i = Str.matched_group 6 input in
  let temperature_to_humidity_i = Str.matched_group 7 input in
  let humidity_to_location_i = Str.matched_group 8 input in
  {
        seeds = List.map int_of_string (Str.split (Str.regexp " ") seeds_i);
        seed_to_soil = parse_map seed_to_soil_i;
        soil_to_fertilizer = parse_map soil_to_fertilizer_i;
        fertilizer_to_water = parse_map fertilizer_to_water_i;
        water_to_light = parse_map water_to_light_i;
        light_to_temperature = parse_map light_to_temperature_i;
        temperature_to_humidity = parse_map temperature_to_humidity_i;
        humidity_to_location = parse_map humidity_to_location_i;
  }

(** [alamanc_lookup map key] looks up [key] in [map].
    It will first check all ranges in [map] to see if [key] is in any of them,
    and if so, return the corresponding value. If not, it will return [key].
 *)
let alamanc_lookup (map : almanac_map) (key : int) : int =
  let found = List.find_opt
                (fun range ->
                  key >= range.source_start && key < range.source_start + range.range
                ) map
  in
  match found with
  | Some value -> value.dest_start + (key - value.source_start)
  | None -> key


(** [find_location almanac seed] finds the location of [seed] in [almanac]. *)
let find_location (almanac : almanac) (seed : int) : int =
  let soil = alamanc_lookup almanac.seed_to_soil seed in
  let fertilizer = alamanc_lookup almanac.soil_to_fertilizer soil in
  let water = alamanc_lookup almanac.fertilizer_to_water fertilizer in
  let light = alamanc_lookup almanac.water_to_light water in
  let temperature = alamanc_lookup almanac.light_to_temperature light in
  let humidity = alamanc_lookup almanac.temperature_to_humidity temperature in
  alamanc_lookup almanac.humidity_to_location humidity

(** [parse_seed_ranges lst] parses a list of seed ranges into a list of tuples.
    It will group every two elements in [lst] into the first being the start of
    the range and the second being the range size. The tuples of the resulting
    list will be a pair of the start and size of the range.
 *)
let rec parse_seed_ranges = function
  | [] -> []
  | s :: r :: rst -> (s, r) :: parse_seed_ranges rst
  | _ -> failwith "Invalid seed range"

class t =
  object (_)
    inherit Day_intf.t 5

    (** [part1 i] is the solution to part 1 of the day 5 puzzle.
        It will find the location of each seed in [i] and return the minimum
        location.
     *)
    method part1 (i : string) : string =
      let almanac = parse_almanac i in
      List.fold_left (fun acc seed ->
          find_location almanac seed |> min acc
        ) max_int almanac.seeds |> string_of_int


    (** [part2 i] is the solution to part 2 of the day 5 puzzle.
        It will first parse the seed ranges, then for each seed range, it will
        find the location of each seed in the range and return the minimum, then
        return the minimum of all the minimums.
     *)
    method part2 (i : string) : string =
      let almanac = parse_almanac i in
      let seed_ranges = parse_seed_ranges almanac.seeds in
      List.fold_left (fun minAcc (s, r) ->
          let locs = List.init r (fun i -> find_location almanac (s + i)) in
          List.fold_left (fun acc loc ->
              min acc loc
            ) minAcc locs
        ) max_int seed_ranges |> string_of_int
  end



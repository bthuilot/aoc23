
(** Advent of Code - Day 2

    https://adventofcode.com/2020/day/2

    This day involves parsing game results and calculating
    first the game IDs of possible games, and then the
    "power" of each game, which is the product of the
    minimum number required of each color cube used in the game.

    I spent a lot of time on the parsing of the game results
    into a common data structure, which resulted in the
    individual parts being fairly simple.

    The first part is a simple filter of the possible games
    based on the maximum number of cubes of each color. If any
    game has more than the maximum, it is not possible.

    The second part is a simple fold over the possible games
    to find the maximum number of cubes of each color used in
    the game, and then multiplying them together.
 *)

(** [cube_reveal] represents the number of cubes of each color
    revealed in a single turn of a game. *)
type cube_reveal = {
    red : int;
    green : int;
    blue : int;
  }

(** [game] represents a single game, with a unique ID and a list
    of [cube_reveal]s. *)
type game = {
    id : int;
    reveals: cube_reveal list;
  }

(** [parse_gameid line] parses the game ID from a line of input. *)
let parse_gameid (line : string) : int =
  let r = Str.regexp {|Game \([0-9]+\):.+|} in
  let _ = Str.string_partial_match r line 0 in
  let game_id = Str.matched_group 1 line in
  int_of_string game_id


(** [parse_reveals line] parses the list of [cube_reveal]s from a line of input. *)
let parse_reveals (line : string) : cube_reveal list =
  let reveals = match String.split_on_char ':' line with
    | [_; reveals] -> reveals |> String.split_on_char ';'
    | _ -> [] in
  List.map (fun reveal ->
      let colors = reveal |> String.split_on_char ',' in
      List.fold_left (fun acc colorS ->
          let color = String.trim colorS in
          let r = Str.regexp {|\([0-9]+\) \([a-z]+\)|} in
          let _ = Str.string_match r color 0 in
          let amt = Str.matched_group 1 color in
          let color = Str.matched_group 2 color in
          match color with
          | "red" -> { acc with red = int_of_string amt }
          | "green" -> { acc with green = int_of_string amt }
          | "blue" -> { acc with blue = int_of_string amt }
          | _ -> acc
        ) { red = 0; green = 0; blue = 0 } colors
    ) reveals


(** [parse_input input] parses the input into a list of [game]s. *)
let parse_input (input : string) : game list =
  let lines = String.split_on_char '\n' input in
  List.map (fun line ->
      let game_id = parse_gameid line in
      let reveals = parse_reveals line in
      { id = game_id; reveals = reveals }
    ) lines

(** [find_possible_games gs] finds the possible games from a list of [game]s. *)
let find_possible_games (gs : game list) =
  let maxRed, maxBlue, maxGreen = 12, 14, 13 in
  List.filter (fun g ->
      List.exists (fun r -> r.red > maxRed || r.green > maxGreen || r.blue > maxBlue) g.reveals |> not
    ) gs


(** [find_game_powers gs] finds the "power" of each game from a list of [game]s. *)
let find_game_powers (gs : game list) =
  List.map (fun g ->
      let (minR, minG, minB) =
        List.fold_left(fun (minR, minG, minB) r ->
            let red = max r.red minR in
            let green = max r.green minG in
            let blue = max r.blue minB in
            (red, green, blue)
          ) (0, 0, 0) g.reveals in
      minR * minG * minB
    ) gs

class t =
  object (_)
    inherit Day_intf.t 2

    (** [part1] finds the sum of the IDs of the possible games. *)
    method part1 (i : string) : string =
      let gs = parse_input i in
      find_possible_games gs |> List.fold_left (fun acc g -> acc + g.id) 0 |> string_of_int

    (** [part2] finds the sum of the "power" of each game. *)
    method part2 (i : string) : string =
      let gs = parse_input i in
        find_game_powers gs |> List.fold_left (+) 0 |> string_of_int
  end



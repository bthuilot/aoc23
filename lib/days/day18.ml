(** Day 18: Lavaduct Lagoon

    https://adventofcode.com/2023/day/18

    Following the trend of 1 easy problem for every hard problem,
    This one took almost no time at all. This relies on the shoelace
    formula and the picks therom, which I learned about when looking
    up stuff for Day 10.

    The first part simply took parsing the directions and distances
    from the input, and converting them into coordinates by starting at
    (0,0) and moving in the given direction for the given distance, for
    each line in the input. Then, the shoelace formula and picks therom
    are used to find the total amount of coordinates inside the polygon, and the perimeter
    of the polygon, which when added produce the result.

    Part 2 was the same thing, just a difference in the parsing of the
    input.
 *)

(** [direction] represents the direction of a dig.
    [North] is up, [South] is down, [West] is left, and [East] is right
    (relative from a top-down view).
 *)
type direction = | North | South | West | East

(** [dig] represents a single dig, which is a direction and a distance. *)
type dig = {
    direction: direction;
    distance: int;
  }

(** [dig_plan] represents a list of digs. *)
type dig_plan = dig list

(** [point] represents a point on a 2D plane. *)
type point = (int * int)

(** [parse_dig_plan_part1] parses the input for part 1 into a [dig_plan].
    The input is a list of digs, where each dig is a direction, a distance,
    and a color. The color is ignored for part 1.
 *)
let parse_dig_plan_part1 (s : string) : dig_plan =
  let re = Str.regexp {|\([UDLR]\) \([0-9]+\) (#[0-9a-f]+)|} in
  let parse_dig (s : string) : dig =
    let _ = Str.string_match re s 0 in
    let direction = match Str.matched_group 1 s with
      | "U" -> North
      | "D" -> South
      | "L" -> West
      | "R" -> East
      | _ -> failwith "Invalid direction"
    in
    let distance = int_of_string (Str.matched_group 2 s) in
    { direction; distance }
  in
  List.map parse_dig (String.split_on_char '\n' s)

(** [parse_dig_plan_part2] parses the input for part 2 into a [dig_plan].
    The input is a list of digs, where each dig the first 5 digits of the hex
    color, and the last digit is the direction.
 *)
let parse_dig_plan_part2 (s : string) : dig_plan =
  let re = Str.regexp {|[UDLR] [0-9]+ (#\([0-9a-f]+\)\([0-3]\))|} in
  let parse_dig (s : string) : dig =
    let _ = Str.string_match re s 0 in
    let hex = Str.matched_group 1 s in
    let distance = int_of_string ("0x" ^ hex) in
    let direction = match Str.matched_group 2 s with
      | "0" -> East
      | "1" -> South
      | "2" -> West
      | "3" -> North
      | _ -> failwith "Invalid direction"
    in
    { direction; distance }
  in
  List.map parse_dig (String.split_on_char '\n' s)

(** [gather_coordinates] gathers all the coordinates from a [dig_plan], assuming the
    digging started at the given point. The coordinates are gathered by starting at
    at the given point, and moving in the given direction for the given distance, for
    each dig in the [dig_plan].
 *)
let rec gather_coordinates (d: dig_plan) ((start_x, start_y): point) : point list =
  let next_point (dir: direction) (distance: int) ((x,y): point) : point =
    match dir with
      | North -> (x, y - distance)
      | South -> (x, y + distance)
      | West -> (x - distance, y)
      | East -> (x + distance, y)
  in
  match d with
  | [] -> [(start_x, start_y)]
  | { direction; distance } :: t ->
     let next_point = next_point direction distance (start_x, start_y) in
     (start_x, start_y) :: gather_coordinates t next_point

(** [shoelace] calculates the area of a polygon given a list of points
    using the shoelace formula. The points must be in clockwise or
    counter-clockwise order.
 *)
let shoelace (points: point list) : int =
  let (x0, y0) = List.hd points in
  let rec shoelace' (points: point list) (acc: int) : int =
    match points with
    | [] -> acc
    | (x1, y1) :: (x2, y2) :: t ->
       shoelace' ((x2, y2) :: t) (acc + (x1 * y2) - (x2 * y1))
    | [(x, y)] -> acc + (x * y0) - (x0 * y)
  in
  (shoelace' points 0 |> abs) / 2

(** [perimeter] calculates the perimeter of a polygon given a [dig_plan].
    The perimeter is calculated by adding up the distance of each dig.
 *)
let perimeter (d: dig_plan) : int =
  List.fold_left (fun acc {distance; _} -> acc + distance) 0 d

(** [picks_therom] calculates the number of points inside a polygon given
    the area and perimeter of the polygon using picks therom.
 *)
let picks_therom (area: int) (perimeter: int) : int =
  (area + 1) - (perimeter / 2 )

class t =
  object (_)
    inherit Day_intf.t 18

    (** [part1] calculates the number of points inside a polygon given
        the area and perimeter of the polygon using picks therom.
        It will return the addition of the perimeter and the number of
        points inside the polygon. The input is parsed using [parse_dig_plan_part1].
     *)
    method part1 (i : string) : string =
      let dig_plan = parse_dig_plan_part1 i in
      let coordinates = gather_coordinates dig_plan (0,0) in
      let area = shoelace coordinates in
      let perimeter = perimeter dig_plan in
      picks_therom area perimeter |> ((+) perimeter) |> string_of_int
    
    (** [part2] calculates the number of points inside a polygon given
        the area and perimeter of the polygon using picks therom.
        It will return the addition of the perimeter and the number of
        points inside the polygon. The input is parsed using [parse_dig_plan_part2].
     *)
    method part2 (i : string) : string =
      let dig_plan = parse_dig_plan_part2 i in
      let coordinates = gather_coordinates dig_plan (0,0) in
      let area = shoelace coordinates in
      let perimeter = perimeter dig_plan in
      picks_therom area perimeter |> ((+) perimeter) |> string_of_int
  end



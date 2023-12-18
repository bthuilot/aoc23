type direction = | Up | Down | Left | Right

type dig = {
    direction: direction;
    distance: int;
    hex_code: string;
  }

type dig_plan = dig list

type point = (int * int)

let parse_dig_plan (s : string) : dig_plan =
  let re = Str.regexp {|\([UDLR]\) \([0-9]+\) (#\([0-9a-fA-F]+\))|} in
  let parse_dig (s : string) : dig =
    let _ = Str.string_match re s 0 in
    let direction = match Str.matched_group 1 s with
      | "U" -> Up
      | "D" -> Down
      | "L" -> Left
      | "R" -> Right
      | _ -> failwith "Invalid direction"
    in
    let distance = int_of_string (Str.matched_group 2 s) in
    let hex_code = Str.matched_group 3 s in
    { direction; distance; hex_code }
  in
  List.map parse_dig (String.split_on_char '\n' s)

let rec gather_coordinates (d: dig_plan) (start_perm: int) ((start_x, start_y): point) : (point list * int) =
  let next_point (dir: direction) (perm: int) (distance: int) ((x,y): point) : (point * int) =
    let next_perm = perm + distance in
    match dir with
      | Up -> (x, y + distance), next_perm
      | Down -> (x, y - distance), next_perm
      | Left -> (x - distance, y), next_perm
      | Right -> (x + distance, y), next_perm
  in
  match d with
  | [] -> [], start_perm
  | { direction; distance; _ } :: t ->
     let (next_point, next_perm) = next_point direction start_perm distance (start_x, start_y) in
     let points, perm = gather_coordinates t next_perm next_point
     in
     (next_point) :: points, perm

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


class t =
  object (_)
    inherit Day_intf.t 18
    method part1 (i : string) : string =
      let dig_plan = parse_dig_plan i in
      let (coordinates, p) = gather_coordinates dig_plan 0 (0,0) in
      let area = shoelace coordinates in
      string_of_int (area + p)
    method part2 (_ : string) : string = "Not Implemented"
  end



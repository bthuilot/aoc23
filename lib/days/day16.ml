(** Day 16: The Floor Will Be Lava

    I love a brute-forceable problem.

    First part was pretty easy. Start from top left with a direction
    and add match cases for each character to determine the next coordinate
    and change in direction. Then just keep track of the coordinates and
    directions you've seen and stop when you see a duplicate. (have to
    also include the direction because you will cross over the same
    coordinate in different directions)

    Part 2 was a layup. Just run the same algorithm from every edge
    coordinate and take the max. I love to try the brute force way
    first and only optimize if it's too slow, and my first attempt
    was plenty fast enough.

 *)

(** [contraption] is a 2D array of characters representing the contraption. *)
type contraption = char array array

(** [direction] is the direction of travel.
    Traveling in a direction means moving one space from that direction.
    U = up, D = down, L = left, R = right
 *)
type direction = U | D | L | R

(** [point] is a coordinate in the contraption. *)
type point = int * int

(** [parse_contraption s] parses the contraption from [s] into a 2D array of characters. *)
let parse_contraption (s : string) : contraption =
  let lines = String.split_on_char '\n' s in
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  let contraption = Array.make_matrix height width '.' in
  List.iteri (fun y line ->
      String.iteri (fun x c ->
          contraption.(y).(x) <- c
        ) line
    ) lines;
  contraption
 
(** [next_movement c (x,y) d] returns a list of all possible next movements from
    coordinate [(x,y)] in direction [d] given contraption [c].
    Each element of the list is a tuple of the next coordinate and direction.
 *)
let next_movement (c: char) ((x,y): point) (d: direction) : (point * direction) list =
  match c, d with
  (* special cases *)
  | '/', R -> [(x, y - 1), U]
  | '/', L -> [(x, y + 1), D]
  | '/', D -> [(x - 1, y), L]
  | '/', U -> [(x + 1, y), R]
  | '\\', R -> [(x, y + 1), D]
  | '\\', L -> [(x, y - 1), U]
  | '\\', D -> [(x + 1, y), R]
  | '\\', U -> [(x - 1, y), L]
  | '|', R | '|', L -> [(x, y - 1), U; (x, y + 1), D]
  | '-', U | '-', D -> [(x - 1, y), L; (x + 1, y), R]
  (* all others, just move in the same direction *)
  | _, U -> [(x, y - 1), U]
  | _, D -> [(x, y + 1), D]
  | _, L -> [(x - 1, y), L]
  | _, R -> [(x + 1, y), R]

(** [mark_energized c (x,y) d] returns a 2D array of directions representing
    the directions that can be traveled from each coordinate in [c] starting
    from [(x,y)] in direction [d].
 *)
let mark_energized (c : contraption) ((startX, startY): int * int) (dir : direction)  : direction list array array =
  (* marked represents the directions that have been traveled from each coordinate.
     This is done to avoid infinite loops. If we have been to a coordinate in a direction,
     we don't want to go there again in that direction.
   *)
  let marked = Array.make_matrix (Array.length c.(0)) (Array.length c) [] in
  let rec mark ((x, y): int * int) (d: direction) =
    if x < 0 || x >= Array.length c.(0) || y < 0 || y >= Array.length c then
      () (* out of bounds *)
    else
      let seen = marked.(y).(x) in
      if List.mem d seen then () (* already been here in this direction *)
      else begin
          marked.(y).(x) <- d :: seen; (* mark as seen *)
          (* get next coordinates and directions, iter on each *)
          next_movement c.(y).(x) (x,y) d |>
            List.iter (fun ((x', y'), d') ->
                mark (x', y') d'
              )
           end
  in mark (startX, startY) dir;
     marked

(** [count_energized m] returns the number of coordinates that have been energized
    in the 2D array of directions [m].
 *)
let count_energized (m: direction list array array) : int =
  Array.fold_left (fun acc row ->
      acc + Array.fold_left (fun acc c -> if List.is_empty c then acc else acc + 1) 0 row
    ) 0 m

(** [edge_starting_points c] returns a list of all the starting points on the
    edges of contraption [c]. Each element of the list is a tuple of the starting
    coordinate and direction.
 *)
let edge_starting_points (c: contraption) : (point * direction) list =
  List.concat [
    List.init (Array.length c.(0)) (fun x -> ((x, 0), D));
    List.init (Array.length c.(0)) (fun x -> ((x, Array.length c - 1), U));
    List.init (Array.length c) (fun y -> ((0, y), R));
    List.init (Array.length c) (fun y -> ((Array.length c.(0) - 1, y), L));
  ]

class t =
  object (_)
    inherit Day_intf.t 16
    
    method part1 (i: string) : string =
      let contraption = parse_contraption i in
      let marked = mark_energized contraption (0, 0) R  in
      let count = count_energized marked in
      string_of_int count
    
    method part2 (i : string) : string =
      let contraption = parse_contraption i in
      edge_starting_points contraption |>
        List.fold_left (fun acc ((x, y), d) ->
            let marked = mark_energized contraption (x, y) d in
            let count = count_energized marked in
            max acc count
          ) 0 |> string_of_int
  end



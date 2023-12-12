(** Day 10: Pipe Maze

    https://adventofcode.com/2023/day/10

    What a problem. This was a MESS for me. Lets say
    a short prayer for me if I ever have to do a graph
    problem in a job interview.

    I figured out the basic DFS/follow the pipe algorithm
    for part 1 pretty quickly, but for the life of me I kept
    messing up the programming. Eventually I got it working
    and moved on to part 2, where I got my ass handed to me.

    After talking with my co-worker because I was pretty stuck,
    he told me he did Point in Polygon algorithm, but I cant seem to
    get it to work. I got it good enough where so long as S isnt actually
    a '|' 'L' or 'J', it works. To fix i would need to re-implement so that I
    calculate S based on the first and last pipes, but im waaayyyy to lazy
    after all the word ive already put into it. Hopefully one day ill get the
    strength to revisit (or figure out what the hell Picks algorithm is)

 *)

(** [pipe_tiles] represents the full matrix *)
type pipe_tiles = char array array

let parse_surface_pipes (input : string) : pipe_tiles =
  input
  |> String.split_on_char '\n'
  |> List.map (fun line ->
      line |> String.to_seq |> Array.of_seq)
  |> Array.of_list

type point = int * int
type dir = Up | Down | Left | Right

(** [PointOrd] is a struct to represent a comparable [point].
    This is done to be able to create a [Set] for [point]
 *)
module PointOrd = struct
  type t = point
  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c
end

(** [PointSet] is a set of [point]s *)
module PointSet = Set.Make(PointOrd)


(** [get_next_point] returns the next point and direction given
    the current pipe and direction. If the pipe is invalid, it
    returns [None]. [heading] is the direction the you were coming
    from, relative the the pipe you are currently. i.e. if you were
    one tile below a '|' pipe, [heading] would be [Up]
 *)
let get_next_point (pipe: char) (heading: dir) ((x,y): point) : (point * dir) option =
  match pipe, heading with
  | '|', Up -> Some ((x, y - 1), Up)
  | '|', Down -> Some ((x, y + 1), Down)
  | '-', Left -> Some ((x - 1, y), Left)
  | '-', Right -> Some ((x + 1, y), Right)
  | 'F', Left -> Some ((x , y + 1), Down)
  | 'F', Up -> Some ((x + 1, y), Right)
  | 'L', Down -> Some ((x + 1, y), Right)
  | 'L', Left -> Some ((x, y - 1), Up)
  | '7', Up -> Some ((x - 1, y), Left)
  | '7', Right -> Some ((x, y + 1), Down)
  | 'J', Down -> Some ((x - 1, y), Left)
  | 'J', Right -> Some ((x, y - 1), Up)
  | _, _ -> None

(** [get_starting_points] returns a list of all the valid starting
    points for the DFS. A valid starting point is a point that is
    within the bounds of the matrix
 *)
let get_starting_points (tiles: pipe_tiles) ((x,y): point) =
  let surrodings = [
      ((x, y - 1), Up);
      ((x, y + 1), Down);
      ((x - 1, y), Left);
      ((x + 1, y), Right);
    ] in
  List.filter (fun ((x, y), _) ->
      x >= 0 && x < Array.length tiles.(0) &&
        y >= 0 && y < Array.length tiles
    ) surrodings


(** [find_loop] returns a [PointSet.t] of all the points in the loop
    if a loop is found. If no loop is found, it returns [None]
 *)
let find_loop (sX, sY: int * int) (tiles: pipe_tiles) : PointSet.t option =
  let init_seen = PointSet.singleton (sX, sY) in
  let rec dfs (seen: PointSet.t) (stack: (point * dir) list) =
    match stack with
    | [] -> None
    | ((x, y), d) :: rest ->
       let chr = tiles.(y).(x) in
       let new_seen = PointSet.add (x, y) seen in
       match get_next_point chr d (x, y) with
       | None -> dfs seen rest (* Invalid pipe *)
       | Some ((x', y'), d') ->
          if x' < 0 || x' >= Array.length tiles.(0) || y' < 0 || y' >= Array.length tiles then
            (* Out of bounds *)
            dfs new_seen rest
          else if tiles.(y').(x') = 'S' then
            (* PointSet.mem (x', y') seen then *)
            (* Found a loop *)
            Some new_seen
          else
            (* Continue searching *)
            dfs new_seen (((x', y'), d') :: rest)
  in
  get_starting_points tiles (sX, sY) |> dfs init_seen

(** [find_start] returns the starting point of the loop *)
let find_start (tiles: pipe_tiles) : point =
  Array.find_mapi (fun y row ->
      Array.find_mapi (fun x c ->
          if c = 'S' then Some (x, y) else None
        ) row) tiles
  |> Option.get


let point_in_polygon (tiles) (loop: PointSet.t) ((x,y): point) : bool =
  Printf.printf "Checking %d %d\n" x y;
  let rec pip ((x', y'): point) (acc: int) =
    if x' < 0 then acc else begin
      let vert =
        match tiles.(y').(x') with
        | '|' | 'L' | 'J'  -> true (* TODO: bug where if S is a '|', 'L', or 'J' it will fail *)
        | _ -> false in
      pip (x' - 1 , y') (if (PointSet.mem (x', y') loop && vert) then acc + 1 else acc)
      end
  in
  (pip (x, y) 0) mod 2 = 1

(* let shoelace (points: point list) : int = *)
(*   let rec shoelace' (acc: int) (points: point list) = *)
(*     match points with *)
(*     | [] | [_]  -> acc *)
(*     | (x0, y0) :: (x1, y1) :: rest -> *)
(*        shoelace' (acc + (x0 * y1) - (x1 * y0)) ((x1, y1) :: rest) *)
(*   in *)
(*   shoelace' 0 points / 2 |> abs *)
  
class t =
  object (_)
    inherit Day_intf.t 10
    
    method part1 (i : string) : string =
      let tiles = parse_surface_pipes i in
      let (sX, sY) = find_start tiles in
      match find_loop (sX, sY) tiles with
      | None -> "No loop found"
      | Some seen -> (PointSet.cardinal seen / 2 ) |> string_of_int
    
    method part2 (i : string) : string =
      let tiles = parse_surface_pipes i in
      let (sX, sY) = find_start tiles in
      match find_loop (sX, sY) tiles with
      | None -> "No loop found"
      | Some seen ->
         List.fold_left
           (fun (acc, r) row ->
             let (acc, _) =
               List.fold_left (
                   fun (acc, c) _ ->
                   let border = PointSet.mem (c,r) seen in
                   
                   let pip = point_in_polygon tiles seen (c, r) in
                   if (not border) && pip then
                     (acc + 1, c + 1)
                   else
                     (acc, c + 1)
                 ) (acc, 0) (Array.to_list row)
             in
             (acc, r + 1)
           ) (0, 0) (Array.to_list tiles) |> fst |> string_of_int
  end


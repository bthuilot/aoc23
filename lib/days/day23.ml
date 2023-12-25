
type direction = North | South | East | West

type tile = Path | Forest | Slope of direction

type hiking_trail = tile array array

type point = int * int

let parse_input (i: string) : hiking_trail =
  let lines = String.split_on_char '\n' i in
  let trail = Array.make_matrix (List.length lines) (String.length (List.hd lines)) Path in
  List.iteri (fun y s ->
      String.iteri (fun x c ->
          match c with
          | '.' -> trail.(y).(x) <- Path
          | '#' -> trail.(y).(x) <- Forest
          | '^' -> trail.(y).(x) <- Slope North
          | 'v' -> trail.(y).(x) <- Slope South
          | '>' -> trail.(y).(x) <- Slope East
          | '<' -> trail.(y).(x) <- Slope West
          | _ -> failwith "Invalid character"
        ) s
    ) lines;
  trail


module PointSet = Set.Make(struct
    type t = point
    let compare = compare
  end)


type travel_direction = Up | Down | Left | Right

let can_travel_part1 (tile: tile) (dir: travel_direction) : bool =
  match tile, dir with
  | Path, _ -> true
  | Forest, _ -> false
  | Slope North, Up -> true
  | Slope South, Down -> true
  | Slope East, Right -> true
  | Slope West, Left -> true
  | _, _ -> false

let can_travel_part2 (tile: tile) (dir: travel_direction) : bool =
  match tile, dir with
  | Forest, _ -> false
  | _, _ -> true

let neighbors_with_slopes (trail: hiking_trail) (seen: PointSet.t) ((x,y): point) : point list =
  let tile = trail.(y).(x) in
  let can_travel (tile: tile) (dir: travel_direction) =
    match tile, dir with
    | Path, _ -> true
    | Forest, _ -> false
    | Slope North, Up -> true
    | Slope South, Down -> true
    | Slope East, Right -> true
    | Slope West, Left -> true
    | _, _ -> false in
  let all_neighbors = match tile with
    | Path -> [(x, y-1), Up; (x, y+1), Down; (x+1, y), Right; (x-1, y), Left]
    | Forest -> [] (* should not happen *)
    | Slope North -> [(x, y-1), Up]
    | Slope South -> [(x, y+1), Down]
    | Slope East -> [(x+1, y), Right]
    | Slope West -> [(x-1, y), Left] in
  List.fold_left (fun acc ((x, y), dir) ->
          if x >= 0 && x < Array.length trail.(0) &&
               y >= 0 && y < Array.length trail &&
                 not (PointSet.mem (x, y) seen) &&
                    can_travel trail.(y).(x) dir
          then (x, y) :: acc
          else acc
        ) [] all_neighbors


let neighbors_no_slopes (trail: hiking_trail) (seen: PointSet.t) ((x,y): point) : point list =
  let tile = trail.(y).(x) in
  let can_travel (tile: tile) (dir: travel_direction) =
    match tile, dir with
    | Forest, _ -> false
    | _, _ -> true in
  let all_neighbors = match tile with
    | Forest -> [] (* should not happen *)
    | _ -> [(x, y-1), Up; (x, y+1), Down; (x+1, y), Right; (x-1, y), Left]
  in
  List.fold_left (fun acc ((x, y), dir) ->
          if x >= 0 && x < Array.length trail.(0) &&
               y >= 0 && y < Array.length trail &&
                 not (PointSet.mem (x, y) seen) &&
                    can_travel trail.(y).(x) dir
          then (x, y) :: acc
          else acc
        ) [] all_neighbors


let find_longest_path (get_neighbors: (hiking_trail -> PointSet.t -> point -> point list)) (trail: hiking_trail) : (point, int) Hashtbl.t =
  let q = Queue.create () in
  let dist = Hashtbl.create (Array.length trail * Array.length trail.(0)) in
  (* let path = PointSet.empty in *)
  let rec loop (q: (point * int * PointSet.t) Queue.t) = (* (path: PointSet.t) = *)
    if Queue.is_empty q then ()
    else
      let ((x, y), path, seen') = Queue.pop q in
      let seen = PointSet.add (x, y) seen' in
      let () = match Hashtbl.find_opt dist (x, y) with
        | Some l -> if l < path then Hashtbl.replace dist (x, y) path else ()
        | None -> Hashtbl.add dist (x, y) path
      in
      let possible_neighbors = get_neighbors trail seen (x, y) in
      List.iter (fun (x, y) ->
          Queue.push ((x, y), path + 1, seen) q
        ) possible_neighbors;
      loop q
  in
  let () = Queue.push ((1, 0), 0, PointSet.empty) q in
  let () = loop q in
  dist
    
let find_longest_path2 (get_neighbors: (hiking_trail -> PointSet.t -> point -> point list)) (trail: hiking_trail) : (point, int) Hashtbl.t =
  let q = Queue.create () in
  let dist = Hashtbl.create (Array.length trail * Array.length trail.(0)) in
  let rec loop (q: (point * int * PointSet.t) Queue.t) =
    if Queue.is_empty q then ()
    else
      print_endline (string_of_int (Queue.length q));
      let ((x, y), path, seen') = Queue.pop q in
      let seen = PointSet.add (x, y) seen' in
      let () = match Hashtbl.find_opt dist (x, y) with
        | Some l -> if l < path then Hashtbl.replace dist (x, y) path else ()
        | None -> Hashtbl.add dist (x, y) path 
      in
      let possible_neighbors = get_neighbors trail seen (x, y) in
      List.iter (fun (x, y) ->
          Queue.push ((x, y), path + 1, seen) q
        ) possible_neighbors;
      loop q
  in
  let () = Queue.push ((1, 0), 0, PointSet.empty) q in
  let () = loop q in
  dist


class t =
  object (_)
    inherit Day_intf.t 23
    method part1 (i : string) : string =
      let trail = parse_input i in
      let dist = find_longest_path neighbors_with_slopes trail in
      Hashtbl.find dist (Array.length trail.(0) - 2, Array.length trail - 1) |> string_of_int
    method part2 (i : string) : string =
      let trail = parse_input i in
      let dist = find_longest_path2 neighbors_no_slopes trail in
      Hashtbl.find dist (Array.length trail.(0) - 2, Array.length trail - 1) |> string_of_int
  end



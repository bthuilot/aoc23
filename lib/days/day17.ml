(** Day 17: Clumsy Crucible

    https://adventofcode.com/2023/day/17

    This one was not good. I realized it was Dijkstra's algorithm, but I didn't know how to
    incorporate the direction/travel limit into the algorithm. I ended up using a 4D array to
    store the minimum heat loss to each point in each direction with each direction having a
    travel limit of 3. This was not a good solution.

    My thought is to instead use a priority queue and store the current direction and travel
    limit in the queue. This will be much more efficient and will not require a 4D array (hopefully).

    As for now, feast your eyes on this monstrosity.

    No doc comments, just spaghetti code.
 *)

type map = int array array

type point = int * int

type heat_losses = int array array

type direction = Up 
               | Down
               | Left
               | Right

let parse (input : string) : map =
  let lines = input |> String.split_on_char '\n' in
  let m = Array.make_matrix (List.length lines) (String.length (List.hd lines)) 0 in
  List.iteri (fun y line -> String.iteri (fun x c -> m.(y).(x) <- (Char.code c) - 48) line) lines;
  m

type state = {
    point : point;
    heat_loss : int;
    dir : direction;
    dir_amt : int;
  }

let next_points ((x, y) : point) : (point * direction) list =
  [
    (x, y - 1), Up;
    (x, y + 1), Down;
    (x - 1, y), Left;
    (x + 1, y), Right;
  ]

let reverse_dir (d : direction) : direction =
  match d with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

let is_in_map (map : map) ((x, y) : point) : bool =
  x >= 0 && y >= 0 && x < Array.length map.(0) && y < Array.length map

let get_dir_num (d : direction) : int =
  match d with
  | Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3

(** [traverse_crucibles start map] will traverse the crucibles in [map]
      starting at [start] and return a [heat_loss] returning the minimum heat loss to each point.
      It is a heavily modified Dijkstra's algorithm. 
 *)
let traverse_crucibles ((start_x, start_y): point) (map : map) : heat_losses =
  let hl = Array.make_matrix (Array.length map) (Array.length map.(0)) None |>
             Array.map (Array.map (fun _ -> Array.make_matrix 4 3 None))
  in
  let rec loop (pq: state Priority_queue.t) : unit =
    match Priority_queue.dequeue pq with
    | None -> ()
    | Some ({point = (x, y); heat_loss; dir; dir_amt}, pq_rst) ->
       if dir_amt > 3 then
         (loop pq_rst)
       else
         let next_pq = match hl.(y).(x).(get_dir_num dir).(dir_amt - 1) with
           | Some d when d <= heat_loss ->
              pq_rst
           | _ -> begin
               hl.(y).(x).(get_dir_num dir).(dir_amt - 1) <- Some heat_loss;
               let next_points = next_points (x, y) |> List.filter (
                                                           fun ((x', y'), d) -> d <> reverse_dir dir
                                                                         && is_in_map map (x', y')
                                                                         && ((d = dir && dir_amt < 3) || d <> dir)
                                                         )
               in
               List.fold_left (fun pq ((x', y'), d) ->
                   let next_hl = heat_loss + map.(y').(x')
                   in
                   Priority_queue.enqueue pq next_hl {
                       point = (x',y');
                       heat_loss = next_hl;
                       dir = d;
                       dir_amt = if d = dir then dir_amt + 1 else 1;
                 }) pq_rst next_points 
             end
         in
         loop next_pq
  in
  loop [
      (0, {
        point = (start_x + 1, start_y);
        heat_loss = map.(start_y).(start_x +1);
        dir = Right;
        dir_amt = 1
      });
      (0, {
        point = (start_x, start_y + 1);
        heat_loss = map.(start_y + 1).(start_x);
        dir = Down;
        dir_amt = 1
      })
    ];
  Array.map (Array.map (fun x ->
                 Array.fold_left (fun acc r ->
                     Array.fold_left (
                         fun acc' x ->
                         match x with
                         | None -> acc'
                         | Some x -> min acc' x
                       ) acc r
                   ) max_int x)) hl

let next_point_dir (amt: int) ((x, y) : point) (dir : direction) : point =
  match dir with
  | Up -> (x, y - amt)
  | Down -> (x, y + amt)
  | Left -> (x - amt, y)
  | Right -> (x + amt, y)

let traverse_crucibles' ((start_x, start_y): point) (map : map) : heat_losses =
  let hl = Array.make_matrix (Array.length map) (Array.length map.(0)) None |>
             Array.map (Array.map (fun _ -> Array.make_matrix 4 10 None))
  in
  let rec loop (pq: state Priority_queue.t) : unit =
    match Priority_queue.dequeue pq with
    | None -> ()
    | Some ({point = (x, y); heat_loss; dir; dir_amt}, pq_rst) ->
         let next_pq = match hl.(y).(x).(get_dir_num dir).(dir_amt - 1) with
           | Some d when d <= heat_loss -> pq_rst
           | _ -> begin
               hl.(y).(x).(get_dir_num dir).(dir_amt - 1) <- Some heat_loss;
               let next_points_s = List.filter ((<>) (reverse_dir dir)) [Up; Down; Left; Right]
                                 |> List.map (fun d ->
                                        if dir <> d then
                                          List.init 4 (fun i -> next_point_dir (i + 1) (x, y) d), d
                                        else
                                         [next_point_dir 1 (x, y) d], d
                                   )
               in
               List.fold_left (fun pq (l, d) ->
                   let ((x', y'), next_hl) = List.fold_left (
                                                 fun (_, hl) (x',y')->
                                                 if is_in_map map (x', y') then
                                                   let next_hl = hl + map.(y').(x')
                                                   in ((x', y'), next_hl)
                                                 else
                                                   ((x', y'), hl)
                                               ) ((x,y), heat_loss) l
                   in
                   let dir_amt' = (if d <> dir then 0 else dir_amt) + List.length l in
                   if is_in_map map (x', y') && dir_amt' <= 10 then
                     Priority_queue.enqueue pq next_hl {
                         point = (x',y');
                         heat_loss = next_hl;
                         dir = d;
                         dir_amt = dir_amt';
                       }
                   else pq
                 ) pq_rst next_points_s
             end
         in
         loop next_pq
  in
  loop [
      (0, {
        point = (start_x + 4, start_y);
        heat_loss = map.(start_y).(start_x + 1) + map.(start_y).(start_x + 2) + map.(start_y).(start_x + 3) + map.(start_y).(start_x + 4);
        dir = Right;
        dir_amt = 4
      });
      (0, {
        point = (start_x, start_y + 4);
        heat_loss =map.(start_y + 1).(start_x) + map.(start_y + 2).(start_x) + map.(start_y + 3).(start_x) + map.(start_y + 4).(start_x);
        dir = Down;
        dir_amt = 4
      })
    ];
  Array.map (Array.map (fun x ->
                 Array.fold_left (fun acc r ->
                     Array.fold_left (
                         fun acc' x ->
                         match x with
                         | None -> acc'
                         | Some x -> min acc' x
                       ) acc r
                   ) max_int x)) hl



class t =
  object (_)
    inherit Day_intf.t 17
    
    method part1 (i : string) : string = 
      let map = parse i in
      let hl = traverse_crucibles (0, 0) map in
      hl.(Array.length map - 1).(Array.length map.(0) - 1) |> string_of_int
    
    method part2 (i : string) : string =
      let map = parse i in
      let hl = traverse_crucibles' (0, 0) map in
      hl.(Array.length map - 1).(Array.length map.(0) - 1) |> string_of_int
  end




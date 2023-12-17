(* open Utils  *)

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

(** [traverse_crucibles start map] will traverse the crucibles in [map]
      starting at [start] and return a [heat_loss] returning the minimum heat loss to each point.
      It is a modified Dijkstra's algorithm.
 *)
let traverse_crucibles ((start_x, start_y): point) (map : map) : heat_losses =
  let hl = Array.make_matrix (Array.length map) (Array.length map.(0)) None |>
             Array.map (Array.map (fun _ -> Array.make 3 None))
  in
  let path = Array.make_matrix (Array.length map) (Array.length map.(0)) None in
  in
  let rec loop (pq: state Priority_queue.t) : unit =
    match Priority_queue.dequeue pq with
    | None -> ()
    | Some ({point = (x, y); heat_loss; dir; dir_amt}, pq_rst) ->
       (* Printf.printf "x: %d, y: %d, heat_loss: %d, dir: %s, dir_amt: %d\n" x y heat_loss ( *)
       (*     match dir with Up -> "up" | Down -> "down" | Left -> "left" | Right -> "right" *)
       (*   ) dir_amt; *)
       if dir_amt > 3 then
         (
           (* Printf.printf "skipping\n"; *)
         loop pq_rst)
       else
         (* let next_hl = heat_loss + map.(y).(x) in *)
         let next_pq = match hl.(y).(x).(dir_amt - 1) with
           | Some d when d <= heat_loss ->
              (* Printf.printf "skippinn\n"; *)
              pq_rst
           | _ -> begin
               (* Printf.printf "setting %d, %d, %d\n" x y next_hl; *)
               hl.(y).(x).(dir_amt - 1) <- Some heat_loss;
               let next_points = next_points (x, y) |> List.filter (
                                                           fun ((x', y'), d) -> d <> reverse_dir dir
                                                                         && is_in_map map (x', y')
                                                                         && ((d = dir && dir_amt < 3) || d <> dir)
                                                         )
               in
               List.fold_left (fun pq ((x', y'), d) ->
                   (* Printf.printf "adding %d, %d, %d, %s, %d\n" (fst p) (snd p) next_hl ( *)
                   (*     match d with Up -> "up" | Down -> "down" | Left -> "left" | Right -> "right" *)
                   (*   ) (if d = dir then dir_amt + 1 else 1); *)
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
                 Array.fold_left (
                     fun acc x ->
                     match x with
                     | None -> acc
                     | Some x -> min acc x
                   ) max_int x)) hl


class t =
  object (_)
    inherit Day_intf.t 17
    method part1 (i : string) : string =
      let map = parse i in
      let hl = traverse_crucibles (0, 0) map in
      hl.(Array.length map - 1).(Array.length map.(0) - 1) |> string_of_int
    method part2 (_ : string) : string = "Not Implemented"
  end



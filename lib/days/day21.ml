
let part1_steps = ref 64

let part2_steps = ref 26501365


type plot = Garden | Rock

type grid = plot array array
type point = int * int


module PointQ = Core.Hash_queue.Make (struct
                type t = point
                let hash = Hashtbl.hash
                let sexp_of_t = Core.sexp_of_pair Core.sexp_of_int Core.sexp_of_int
                let compare (x1, y1) (x2,y2) =
                  match Int.compare x1 x2 with
                    | 0 -> Int.compare y1 y2
                    | x -> x
              end)


let parse_grid (g: string) : (grid *  point)  =
  let lines = String.split_on_char '\n' g in
  let grid = Array.make_matrix (List.length lines) (String.length (List.hd lines)) Garden in
  let start = ref (0,0) in
  List.iteri (fun i line ->
      String.iteri (fun j c ->
          let plot = match c with
            | 'S' -> start := (i,j); Garden
            | '#' -> Rock
            | _ -> Garden
          in
          grid.(i).(j) <- plot
        ) line
    ) lines;
  grid, !start

let out_of_grid (grid: grid) (x,y) =
  x < 0 || x > Array.length grid || y < 0 || y > Array.length grid.(0)

let wrap_coordinate (x,y) (length, width) =
  let x' = if x < 0 then length + (x mod length - 1) else
             if x >= length then x mod length else x in
  let y' = if y < 0 then width + (y mod width - 1) else
             if y >= width then y mod width else y in
  (x',y')

let traverse (max_steps: int) (grid: grid) (start: point) : (point, int) Hashtbl.t =
  let q: (point *int) Queue.t = Queue.create () in
  Queue.add (start, 0) q;
  let seen = Hashtbl.create (Array.length grid * Array.length grid.(0)) in
  let rec loop () =
    match Queue.take_opt q with
    | None -> seen
    | Some ((x,y), steps) ->
       if out_of_grid grid (x,y) || Hashtbl.mem seen (x,y) then loop ()
      else begin
        let plot = grid.(x).(y) in
        match plot with
        | Rock -> loop ()
        | Garden ->
          if steps > max_steps then ()
          else begin
              let steps' = steps + 1 in
              let neighboors = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)] in
              List.iter (fun (n_x,n_y) ->
                  Queue.add ((n_x,n_y), steps') q
                ) neighboors;
              Hashtbl.add seen (x,y) steps
            end;
          loop ()
      end
  in
  loop ()


class t =
  object (_)
    inherit Day_intf.t 21
    method part1 (i : string) : string =
      let grid, start = parse_grid i in
      let seen = traverse !part1_steps grid start in
      Hashtbl.fold (fun _ steps acc -> if (steps mod 2) = (!part1_steps mod 2)then acc + 1 else acc) seen 0 |> string_of_int
    method part2 (i : string) : string =
      let grid, start = parse_grid i in
      let seen = traverse !part2_steps grid start in
      Hashtbl.fold (fun _ steps acc ->
          if (steps mod 2) = (!part2_steps mod 2) then acc + 1 else acc
        ) seen 0 |> string_of_int
  end



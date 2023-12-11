
type pipe_tiles = char array array

let parse_surface_pipes (input : string) : pipe_tiles =
  input
  |> String.split_on_char '\n'
  |> List.map (fun line ->
      line |> String.to_seq |> Array.of_seq)
  |> Array.of_list

type point = int * int
type dir = Up | Down | Left | Right

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

let find_loop_length (sX, sY: int * int) (tiles: pipe_tiles) : int option =
  let visited = Hashtbl.create (Array.length tiles * Array.length tiles.(0)) in
  Hashtbl.add visited (sX, sY) 0;
  let rec dfs (trail: point list) (stack: (point * dir) list) =
    match stack with
    | [] -> None
    | ((x, y), d) :: rest ->
       let chr = tiles.(y).(x) in
       let new_trail = begin
           Hashtbl.add visited (x, y) (List.length trail);
           (x, y) :: trail
         end in
       match get_next_point chr d (x, y) with
       | None -> dfs trail rest (* Invalid pipe *)
       | Some ((x', y'), d') ->
          if x' < 0 || x' >= Array.length tiles.(0) || y' < 0 || y' >= Array.length tiles then
            (* Out of bounds *)
            dfs trail rest
          else if Hashtbl.mem visited (x', y') then
            (* Found a loop *)
            Some ((List.length new_trail + Hashtbl.find visited (x', y')) / 2)
          else
            (* Continue searching *)
            dfs new_trail (((x', y'), d') :: rest)
  in
  get_starting_points tiles (sX, sY) |> dfs [(sX, sY)] 


class t =
  object (_)
    inherit Day_intf.t 10
    
    method part1 (i : string) : string =
      let tiles = parse_surface_pipes i in
      let (sX, sY) = Array.find_mapi (fun y row ->
                         Array.find_mapi (fun x c ->
                             if c = 'S' then Some (x, y) else None
                           ) row) tiles
                     |> Option.get
      in
      find_loop_length (sX, sY) tiles |> Option.get |> string_of_int
    
    method part2 (_ : string) : string = "Not Implemented"
  end



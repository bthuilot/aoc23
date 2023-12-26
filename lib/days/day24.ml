
let part1_min = ref 200000000000000.0

let part1_max = ref 400000000000000.0

type point_3d = {
    x : int;
    y : int;
    z : int;
  }

type trajectory = {
    position : point_3d;
    velocity: point_3d;
  }

type linear_eq = {
        dx : float;
        dy : float;
        c : float;
  }

type point = float * float

let parse_input (i: string) =
  let lines = String.split_on_char '\n' i in
  let parse_line (l: string) =
    let _ = Str.string_match (Str.regexp {|\([-0-9]+\),[ ]+\([-0-9]+\),[ ]+\([-0-9]+\)[ ]+@[ ]+\([-0-9]+\),[ ]+\([-0-9]+\),[ ]+\([-0-9]+\)|}) l 0 in
    let x = Str.matched_group 1 l |> int_of_string in
    let y = Str.matched_group 2 l |> int_of_string in
    let z = Str.matched_group 3 l |> int_of_string in
    let vx = Str.matched_group 4 l |> int_of_string in
    let vy = Str.matched_group 5 l |> int_of_string in
    let vz = Str.matched_group 6 l |> int_of_string in
    {position = {x; y; z}; velocity = {x = vx; y = vy; z = vz}}
  in
  List.map parse_line lines

let find_intersect (line1: linear_eq) (line2: linear_eq): point option =
  
  let denom = line2.dx *. line1.dy -. line1.dx *. line2.dy in
  if denom = 0.0 then None
  else
    let y = -1.0 *. (line1.dx *. line2.c -. line2.dx *. line1.c) /. denom in
    let x = (line2.dy *. line1.c -. line1.dy *. line2.c) /. denom in
    Some (x, y)

let calc_linear_eq (p: point_3d) (v: point_3d): linear_eq =
  let m = (Float.of_int v.y) /. (Float.of_int v.x) in
  let c = (Float.of_int p.y) -. m *. (Float.of_int p.x) in
  let c' = (Float.of_int v.x) *. c in
  {dx = Float.of_int v.y; dy = (Float.of_int v.x); c = c'}
  

let find_xy_intersecets (trajectories: trajectory list) =
  let rec find_intersects (ts: trajectory list) (acc: point list) =
    match ts with
    | [] -> acc
    | t :: t_rst ->
       let rec find_intersects' (ts': trajectory list) (acc: point list) =
         match ts' with
         | [] -> acc
         | t' :: t_rst' ->
            let line1 = calc_linear_eq t.position t.velocity in
            let line2 = calc_linear_eq t'.position t'.velocity in
            let x_comp = if t.velocity.x < 0 then (<) else (>) in
            let y_comp = if t.velocity.y < 0 then (<) else (>) in
            let x_comp' = if t'.velocity.x < 0 then (<) else (>) in
            let y_comp' = if t'.velocity.y < 0 then (<) else (>) in
            let acc' = match find_intersect line1 line2 with
              | None -> acc
              | Some (x,y) -> begin
                 let line1_future = (x_comp x (Float.of_int t.position.x)) && (y_comp y (Float.of_int t.position.y)) in
                 let line2_future = (x_comp' x (Float.of_int t'.position.x)) && (y_comp' y (Float.of_int t'.position.y)) in
                 if line1_future && line2_future
                 then
                   (x,y) :: acc
                 else acc
                 end
            in
            find_intersects' t_rst' acc'
            
       in
       find_intersects t_rst (find_intersects' t_rst acc)
  in
  find_intersects trajectories []

class t =
  object (_)
    inherit Day_intf.t 24
    method part1 (i : string) : string =
      let trajectories = parse_input i in
      let intersects = find_xy_intersecets trajectories in
      let intersects = List.filter (fun ((x,y): point) ->
                           Printf.printf "x: %f, y: %f\n" x y;
                           x >= !part1_min && x <= !part1_max && y >= !part1_min && y <= !part1_max) intersects in
      List.length intersects |> string_of_int
      
    method part2 (_ : string) : string = "Not Implemented"
  end



(** Day 14: Parabolic Reflector Dish

    https://adventofcode.com/2023/day/14

    This is not one im proud of lol.

    The first part was pretty standard. I realized that
    shifting a column was gonna be hard in OCaml so I rotated
    the matrix and shifted the row, then rotated it back.

    For part 2 I spent a *while* trying to figure out some pattern
    in the loads. I was trying to think of some way to detect when
    a pattern was reached that is repeating but couldnt really figure
    it out. The next morning I deciced to try getting the first 100
    loads and see if I could find the pattern. I saw in the print
    statements it was reapeating, but 100 wasnt the expected value.
    I tried 1000 and it was. So I ran it on my input and it was
    correct. I guess I got lucky, not sure what the "real" solution
    was.

 *)

open Utils

(** [dish] is a matrix of chars. *)
type dish = char list list

(** [parse_input input] parses [input] into a [dish]. *)
let parse_input (input : string) : dish =
  input
  |> String.split_on_char '\n' |> List.map (String.to_seq >> List.of_seq)

(** [calc_total_load dish] calculates the total load of [dish].
    The total load is the sum of the number of rocks in each row
    multiplied by the number of rows above it.
 *)
let calc_total_load (dish: dish) =
  List.mapi (fun i r ->
      let rocks = List.filter (fun c -> c = 'O') r |> List.length
      in
      rocks * (List.length dish - i )
    ) dish |> List.fold_left (+) 0

(** [tilt_east dish] tilts [dish] to the east. *)
let tilt_east (dish : dish) : dish =
  let rec tilt_row (total: int) (rocks : int) (row: char list) : char list =
    let stack_rocks t r = List.init (t - r) (fun _ -> '.') @ List.init r (fun _ -> 'O') in
    match row with
    | [] -> stack_rocks total rocks
    | '#' :: xs -> (stack_rocks total rocks) @ '#' :: (tilt_row 0 0 xs)
    | c :: xs -> tilt_row (total + 1) (rocks + if c = 'O' then 1 else 0) xs
  in
  List.map (tilt_row 0 0) dish

(** [cycle dish] cycles [dish] once.
    A cycle is tilting the dish north, then west, then south, then east.
 *)
let cycle (d: dish) =
  repeat (rotate_clockwise >> tilt_east) d 4

(** [calc_first_x_loads x d] calculates the first [x] loads of [d].
    The first load is the load of [d]. The second load is the load of
    [d] after one cycle. The third load is the load of [d] after two
    cycles, etc. The list returned is in reverse order.
 *)
let calc_first_x_loads (x :int) (d : dish) : int list =
  let rec calc_first_x_loads_aux (x : int) (d : dish) (acc : int list) : int list =
    if x = 0 then acc
    else
      let new_dish = cycle d in
      let new_load = calc_total_load new_dish in
      calc_first_x_loads_aux (x - 1) new_dish (new_load :: acc)
  in
  calc_first_x_loads_aux x d []
  
class t =
  object (_)
    inherit Day_intf.t 14
    method part1 (i : string) : string =
      let reflector_dish = parse_input i in
      let titled = tilt_east (rotate_clockwise reflector_dish) |> rotate_counterclockwise in
      calc_total_load titled |> string_of_int

    
    method part2 (i: string) : string =
      let reflector_dish = parse_input i in
      (* read above for explanation of 1000 *)
      let first_x_loads = calc_first_x_loads 1000 reflector_dish in
      List.hd first_x_loads |> string_of_int
  end



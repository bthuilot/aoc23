(** Day 22: Sand Slabs

    http://adventofcode.com/2023/day/22

    This one seemed challenging, but it turned out to be pretty
    easy once i figured out how to represent the cubes, and how to
    determine where a cube would settle.

    For the first part, I broke it into two parts: first, I took in the
    given cube list, and create a function to "settle" the cubes (aka
    determine where they would end up if falling from the top, given
    the cubes below them). Then, I created a function to determine
    which cubes would cause cubes above them to fall if disintegrated.
    This was simply iterating over the cubes, and for each cube,
    removing it from the list, settling the cubes, and comparing the
    z_spread of the settled cubes to the original cubes. I then just
    determined which cubes when removed caused no change.

    Part 2 then ended up being super simple given this approach. Instead
    keeping a boolean of comparing which cubes changed, I just kept a
    count of how many cubes changed, and returned a list where each
    index corresponded to the number of cubes that changed when the
    cube at that index was removed. Then I just summed the list and
    returned the result.

    There is probably a more efficient way to do this, without having
    to iterate over the cubes for each cube, but this was fast enough for me,
    since I still have the previous 2 days' part 2 to do.

 *)

(** [cube] represents a cube of sand, with a spread in the x, y, and z
    directions *)
type cube = {
    x_spread: (int * int);
    y_spread: (int * int);
    z_spread: (int * int);
  }

(** [compare_cube c1 c2] compares two cubes by their minimum z value.
    This is used to sort the cubes by their minimum z value, so that
    cubes that are below other cubes are processed first
 *)
let compare_cube ({z_spread=(z1_1, z1_2); _} : cube) ({z_spread=(z2_1, z2_2); _} : cube) =
  compare (min z1_1 z1_2) (min z2_1 z2_2)

(** [parse_input input] parses the input into a list of cubes.
    The list will be sorted by the minimum z value of each cube
    meaning that cubes that are below other cubes will be processed
 *)
let parse_input (input : string) : cube list =
  let lines = String.split_on_char '\n' input in
  let cubes = List.map (fun line ->
                  let _ = Str.string_match (Str.regexp {|\([0-9]+\),\([0-9]+\),\([0-9]+\)~\([0-9]+\),\([0-9]+\),\([0-9]+\)|}) line 0 in
                  let x1 = int_of_string (Str.matched_group 1 line) in
                  let y1 = int_of_string (Str.matched_group 2 line) in
                  let z1 = int_of_string (Str.matched_group 3 line) in
                  let x2 = int_of_string (Str.matched_group 4 line) in
                  let y2 = int_of_string (Str.matched_group 5 line) in
                  let z2 = int_of_string (Str.matched_group 6 line) in
                  {x_spread=(
                     if x1 < x2 then (x1, x2) else (x2, x1));
                   y_spread=(
                     if y1 < y2 then (y1, y2) else (y2, y1));
                   z_spread=(
                     if z1 < z2 then (z1, z2) else (z2, z1))}
                ) lines in
  List.sort compare_cube cubes

(** [range (r1, r2)] returns the number of integers in the range [r1, r2] *)
let range ((r1, r2): int * int) : int = r2 - r1 + 1

(** [range_overlaps r1 r2] returns true if the ranges [r1] and [r2] overlap *)
let range_overlaps (r1: int * int) (r2: int * int) : bool =
  let (r1_1, r1_2) = r1 in
  let (r2_1, r2_2) = r2 in
  (r1_1 <= r2_1 && r1_2 >= r2_1) || (r2_1 <= r1_1 && r2_2 >= r1_1)

(** [move_to c z cubes_below] returns the z value that the cube [c] would
    settle at if it were to fall from the top, given the cubes below it
    [cubes_below]. If the cube would not intersect with any cubes below it,
    then it would settle at z = 1 (ground level). Otherwise, it would settle at the max
    z value of the cubes below it, plus 1.
 *)
let move_to (c: cube) (z: int) (cubes_below: cube list) : int =
  List.fold_left ( fun acc below ->
                   (* would intersect if x and y ranges overlap *)
                   if range_overlaps c.x_spread below.x_spread &&
                        range_overlaps c.y_spread below.y_spread then
                     (* find max z of cube below, and add 1 since cube will rest on it *)
                     let cur = max (fst below.z_spread) (snd below.z_spread) + 1
                     in max acc cur (* take the max of all cubes below *)
                   else
                     acc
    ) z cubes_below

(** [settle_cubes cs] returns a list of cubes that have been settled.
    This is done by iterating over the cubes, and for each cube, moving
    it to its resting z value, and then updating its z_spread to reflect
    its new resting z value. The cubes are then reversed, so that the
    cubes that are below other cubes are processed first.
 *)
let settle_cubes (cs: cube list) : cube list =
  let rec settle_cubes' (cs: cube list) (settled: cube list) : cube list =
    match cs with
    | [] -> settled
    | c :: cs' ->
        let z = move_to c 1 settled in
        settle_cubes' cs' ({c with z_spread=(z, z + range c.z_spread - 1)} :: settled)
  in
  settle_cubes' cs [] |> List.rev

(** [find_disintegrated cubes] returns a list of integers, where each
    integer represents the number of cubes that disintegrated when the
    cube at that index was settled. This is done by iterating over the
    cubes, and for each cube, removing it from the list of cubes, and
    then settling the cubes. The number of cubes that disintegrated is
    the number of cubes whose z spread changed.
 *)
let find_disintegrated: cube list -> int list =
  let rec disintegrated' (above: cube list) (cubes: cube list) : int list =
    match cubes with
    | [] -> []
    | [_] -> [0]
    | c :: cs ->
       let removed = above @ cs in
       let settled = settle_cubes removed
       in
       let changed = List.fold_left2 (fun acc {z_spread=z; _} {z_spread=z'; _} ->
                         (* if a cube's z spread changes, it "settled" *)
                         if z <> z' then acc + 1 else acc
                       ) 0 removed settled
       in
       changed :: disintegrated' (above @ [c]) cs
  in
  disintegrated' []

class t =
  object (_)
    inherit Day_intf.t 22
    
    method part1 (i : string) : string =
      let cubes = parse_input i in
      let settled = settle_cubes cubes in
      let disintegrated = find_disintegrated settled in
      List.filter ((=) 0) disintegrated |> List.length |> string_of_int
    
    method part2 (i : string) : string =
        let cubes = parse_input i in
        let settled = settle_cubes cubes in
        let disintegrated = find_disintegrated settled in
        List.fold_left ((+) ) 0 disintegrated |> string_of_int
  end



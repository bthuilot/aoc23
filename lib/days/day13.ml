(** Day 13: Point of Incidence

    https://adventofcode.com/2023/day/13

    This day ended up being a lot easier than I initially thought.
    My ah-ha moment was realizing that all I had to do was figure out
    how to calculate where the mirror was for the rows, and if there is
    none, transpose the matrix and try again (since now columns are rows).

    For part 1 I first did rows, where I would pick an "inflection point"
    and determine if the one row back is equal to one row in-front.
    If it is, I keep checking one more row up and down until I hit the end on one side.
    If I don't find a match, I transpose the matrix and try again.

    For part 2 I did the same thing, but instead of checking for equality,
    I find the number of differences between each pair of rows and return
    the sum of those differences. If the sum is 1, then I found the mirror.
    This meant I could re-use the solution for part 1, but just change the
    function to set the "tolerance" (amount of differences allowed) to 0.
 *)

(** [mirror] is a list of lists of chars.
    I represents the "pattern" of the mirror.
 *)
type mirror = char list list

(** [transpose] transposes a matrix.
    This is used to convert rows to columns and vice versa.
 *)
let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)

(** [parse_mirrors] parses the input into a list of mirrors.
    Each mirror is a list of lists of chars.
    Each list of chars represents a row in the mirror.
 *)
let parse_mirrors (i : string) : mirror list =
  let groups = Str.split (Str.regexp "\n\n") i in
  let parse_group (g : string) : mirror =
    let lines = Str.split (Str.regexp "\n") g in
    List.map (fun l -> List.init (String.length l) (String.get l)) lines
  in
  List.map parse_group groups

(** [find_inflection_point tolerance m] finds the inflection point of the mirror [m].
    [tolerance] is the amount of differences allowed between rows.
    If [tolerance] is 0, then the rows must be equal.
    If [tolerance] is 1, then the rows must have 1 difference.
    If [tolerance] is 2, then the rows must have 2 differences.
    etc.
 *)
let find_inflection_point (tolerance : int) (m : mirror) : int option =
  (* diff_amt counts the number of differences between two sets of rows. *)
  let rec diff_amt (front: mirror) (back : mirror) : int =
    match front, back with
    | [], _  -> 0 
    | _, []  -> 0
    | f :: fs, b :: bs ->
       let amt = List.fold_left2
                   (fun acc f' b' -> acc + if f' = b' then 0 else 1)
                   0 f b in
       amt + diff_amt fs bs
  in
  (* row mirror helper function iterates through every "reflection point" of a mirror *)
  let rec find_row_mirror' (front : mirror) (back : mirror) (i : int) : int option =
    match front, back with
    | [], _  -> None
    | _, []  -> None
    | _, b :: bs ->
       if diff_amt front back = tolerance then Some i
       else find_row_mirror' (b :: front) (bs) (i + 1) in
  find_row_mirror' [(List.hd m)] (List.tl m) 1


class t =
  object (_)
    inherit Day_intf.t 13
    
    method part1 (i : string) : string =
      let mirrors = parse_mirrors i in
      let f = find_inflection_point 0 in
      List.fold_left (fun acc m ->
          let value = match f m with
            | Some i -> i * 100
            | None -> match f (transpose m) with
                      | Some i -> i
                      | None -> 0
          in
          acc + value
        ) 0 mirrors |> string_of_int

    method part2 (i : string) : string =
      let mirrors = parse_mirrors i in
      let f = find_inflection_point 1 in
      List.fold_left (fun acc m ->
          let value = match f m  with
            | Some i -> i * 100
            | None -> match f (transpose m) with
                      | Some i -> i
                      | None -> 0
          in
          acc + value
        ) 0 mirrors |> string_of_int
  end



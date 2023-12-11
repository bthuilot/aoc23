(** Day 11: Cosmic Expansion

    This one was a easy one (thank god after yesterday's mess).
    I simply went through the input and found all the galaxies.
    every time I found a galaxy I added it to a list of galaxies,
    and removed the row and column from the set of empty rows and columns
    indices. Then I expanded the "galaxies" by, for each empty row and column
    (in ascending order), adding the amount of expansion to the row or column
    of the galaxy if it was greater than the empty row or column index.

    Then I found all the manhattan distances between all the galaxies and summed them.
    I did this for both parts 1 and 2, only changing the espansion amount for part 2.
 *)

(** [point] is a point in the image *)
type point = int * int

(** [parse_image input] parses the input into a list of lists of characters *)
let parse_image (input : string) : char list list =
  input
  |> String.split_on_char '\n' |> List.map (fun s -> String.to_seq s |> List.of_seq)

(** [IS] is a set of integers *)
module IS = Set.Make(Int)

(** [init_empty_set len] initializes a set of integers from 0 to [len] *)
let init_empty_set (len : int) : IS.t =
  let rec init_empty_set' (s : IS.t) (i : int) : IS.t =
    if i < 0 then s else init_empty_set' (IS.add i s) (i - 1)
  in
  init_empty_set' IS.empty (len - 1)

(** [sort_set_asc s] returns [s]'s elements in ascending order *)
let sort_set_asc (s : IS.t) : int list =
  IS.elements s |> List.sort (fun a b -> compare a b |> ( * ) (-1))

(** [expand_galaxies amt empty_rows empty_cols galaxies] expands the galaxies
    by [amt] in each direction, and returns the expanded galaxies *)
let expand_galaxies (amt : int) (empty_rows: IS.t) (empty_cols: IS.t) (galaxies: point list) : point list =
  let expanded_rows = List.fold_left (fun acc r ->
                          List.map (fun (x, y) ->
                              if y > r then (x, y + amt) else (x, y)
                            ) acc
                        ) galaxies (sort_set_asc empty_rows) in
  let expanded_rows_and_cols = List.fold_left (fun acc c ->
                                   List.map (fun (x, y) ->
                                       if x > c then (x + amt, y) else (x, y)
                                     ) acc
                                 ) expanded_rows (sort_set_asc empty_cols) in
  expanded_rows_and_cols

(** [calc_pair_dist p1 p2] calculates the manhattan distance between [p1] and [p2] *)
let calc_pair_dist (p1 : int * int) (p2 : int * int) : int =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  abs (x1 - x2) + abs (y1 - y2)

(** [decode_image img] decodes the image into a list of galaxies, and the set of empty rows and columns
    [galaxies] is a list of points, [empty_rows] is a set of empty rows indices, and [empty_cols] is a set of empty columns
    indices.
 *)
let decode_image (img: char list list) : (point list * IS.t * IS.t) =
  let xLen, yLen = List.length img, List.length (List.hd img) in
  let (empty_rows, empty_cols) = init_empty_set yLen, init_empty_set xLen in
  
  (* decode_image' will decode the first row finding all galaxies and removing known non-empty cols/rows.
     Then it recursively call itself for on the rest of the rows, passing the updated list of galaxies
     and empty rows/cols. [r] is the row index, [img] is the images, and [acc] is the accumulator, which is a tuple
     of the list of galaxies, the set of empty rows, and the set of empty columns
   *)
  let rec decode_image' (r : int) (img : char list list) (acc : (point list * IS.t * IS.t)) : (point list * IS.t * IS.t) =
    
    (* decode_row decodes a row, adding galaxies to the list of galaxies, and removing
        the row and column from the set of empty rows and columns.
        [c] is the column index, [row] is whats remaing of the row, and [acc] is the accumulator, which is a tuple
        of the list of galaxies, the set of empty rows, and the set of empty columns
     *)
    let rec decode_row (c : int) (row : char list) (acc : point list * IS.t * IS.t) : (point list * IS.t * IS.t) =
      let galaxies, empty_rows, empty_cols = acc in
      match row with
      | [] -> acc
      | chr :: chrs ->
         match chr with
         | '#' -> decode_row (c + 1) chrs ((c, r) :: galaxies,
                                           IS.remove r empty_rows,
                                           IS.remove c empty_cols)
         | _ -> decode_row (c + 1) chrs acc
    in
    match img with
    | [] -> acc
    | col :: cols -> decode_image' (r + 1) cols (decode_row 0 col acc)
  in
  decode_image' 0 img ([], empty_rows, empty_cols)

(** [find_all_dists galaxies acc] finds all the manhattan distances between all the galaxies
    and returns them in a list
 *)
 let rec find_all_dists (galaxies : (int * int) list) (acc : int list) : int list =
   match galaxies with
   | [] -> acc
   | g :: gs ->
      let dists = List.map (fun g' -> calc_pair_dist g g') gs in
      find_all_dists gs (List.append acc dists)


class t =
  object (_)
    inherit Day_intf.t 11
    method part1 (i : string) : string =
      let expand_amt = 1 in
      let img = parse_image i in
      let (galaxies, empty_rows, empty_cols) = decode_image img in
      let expanded = expand_galaxies expand_amt empty_rows empty_cols galaxies in
      let all_dists = find_all_dists expanded [] in
      List.fold_left ((+)) 0 all_dists |> string_of_int

    
    method part2 (i : string) : string =
      let expand_amt = (1_000_000 - 1) in
      let img = parse_image i in
      let (galaxies, empty_rows, empty_cols) = decode_image img in
      let expanded = expand_galaxies expand_amt empty_rows empty_cols galaxies in
      let all_dists = find_all_dists expanded [] in
      List.fold_left ((+)) 0 all_dists |> string_of_int
   
  end


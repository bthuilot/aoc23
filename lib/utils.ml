
(** Utility functions *)



(** [>>] is the composition operator.
    [f >> g] is the function [x -> g (f x)].
    [f >> g >> h] is the function [x -> h (g (f x))].
    [f >> g >> h >> k] is the function [x -> k (h (g (f x)))].
    etc.
 *)
let (>>) f g x = g (f x)

(** [drop i l] returns the list [l] without its first [i] elements. *)
let rec drop i l =
  if i = 0 then l else drop (i - 1) (List.tl l)

(** [take i l] returns the first [i] elements of the list [l]. *)
let rec take i l =
  if i = 0 then [] else (List.hd l) :: take (i - 1) (List.tl l)


(** [transpose] transposes a matrix.
    This is used to convert rows to columns and vice versa.
 *)
let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows  -> 
      List.map List.hd rows :: transpose (List.map List.tl rows)


(** [rotate_clockwise] rotates a matrix clockwise.
 *)
let rotate_clockwise (matrix: 'a list list): 'a list list =
  List.map List.rev (transpose matrix)

(** [rotate_counterclockwise] rotates a matrix counterclockwise.
 *)
let rotate_counterclockwise (matrix: 'a list list): 'a list list =
  List.rev (transpose matrix)

(** [repeat f x n] applies [f] to [x] [n] times.
    [repeat f x 0] is [x].
    [repeat f x 1] is [f x].
    [repeat f x 2] is [f (f x)].
    [repeat f x 3] is [f (f (f x))].
    etc.
 *)
let repeat f x n =
  let rec repeat_aux f x n acc =
    if n = 0 then acc
    else repeat_aux f x (n - 1) (f acc)
  in
  repeat_aux f x n x
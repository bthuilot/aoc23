
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
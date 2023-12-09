
(** Utility functions *)



(** [>>] is the composition operator.
    [f >> g] is the function [x -> g (f x)].
    [f >> g >> h] is the function [x -> h (g (f x))].
    [f >> g >> h >> k] is the function [x -> k (h (g (f x)))].
    etc.
 *)
let (>>) f g x = g (f x)
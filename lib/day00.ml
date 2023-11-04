open! Core
(* Day 0: Template
   -------------------------------------
   This module serves as the example template that all other days are based on.
   It is not intended to be run, but rather to be used as a template for
   creating new days.
 *)
(* Released under the GNU Public License *)

class t =
  object (_)
    inherit Day.t 0
    (** [part1 input] is the solution to part1 of this day's puzzle.
        [input] is the input data, which is capitlaized.
     *)
    method part1 (input : string) : string = input
    (** [part2 input] is the solution to part2 of this day's puzzle.
        [input] is the input data, which is capitlaized.
     *)
    method part2 (input : string) : string = String.uppercase input
  end

let%test_unit "part1" =
  [%test_result:string] ((new t)#part1 "test") ~expect:"test"

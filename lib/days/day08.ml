(** Day 8: Haunted Wasteland

    https://adventofcode.com/2023/day/8

    This one annoyed me a bit, since the "solution" to part 2 relies on
    something that is not mentioned in the problem description, and a valid
    input could be created that would break the solution. But ill get back to
    this point.

    I started off part 1 by doing the (what i thought was) the
    obvious thing: Hashmap of nodes to their left and right neighbors,
    and then a recursive function to navigate the network.  This
    worked fine for part 1, but part 2 was a bit more tricky. I first
    implemented it that would perform the part 1 algorithm for each
    of the starting nodes at the same time until all reached one that ended
    with Z. This needless to say was extremely slow.

    While trying to debug I noticied the same node ending in Z appeared for
    each of the starting nodes. As I started to brain storm how I could use this
    I remembered trying to do GCD and LCM in the previous Day 7 problem. I realized
    that since each starting node had a repeating pattern of a set number of sets
    of instructions before reaching the end, I could just find the LCM of all of
    the starting nodes repeating patterns and that would be the number of steps
    it would take for all of them to reach the end at the same time.

    This is where my annoyance with the problem comes in. The problem description
    does not mention that all of the starting nodes have the same repeating pattern,
    and that there is only one node that ends in Z in that pattern. If there were multiple nodes
    that ended in Z in the pattern, or if the solution did not loop, the solution would not work.
    I guess that its not unreasonable to assume that the solution has some hidden pattern, but
    I wouldnt have thought to look for it if I hadnt noticed it while debugging.
    Note for the future I guess
 *)

(** [network] is a hashtable of nodes to their left and right neighbors *)
type network = (string, (string * string)) Hashtbl.t

(** [map] is the parsed input
    [network] is a hashtable of nodes to their left and right neighbors
    [instructions] is the instructions to navigate the network
    [a_ending] is the list of nodes that end in A (used for part 2)
 *)
type map = {
    network : network;
    instructions : string;
    a_ending : string list;
  }

(** [parse_input i] parses the string input [i] into a [map] *)
let parse_input (i : string) : map =
  let split = Str.split (Str.regexp "\n\n") i in
  let instructions = List.hd split in
  let nodes_str = List.tl split |> List.hd |> Str.split (Str.regexp "\n") in
  let network = Hashtbl.create (List.length nodes_str) in
  (* this is a bit confusing, but basically we are folding over the nodes_str
     and adding them to the network hashtable, and if the node ends in A we
     add it to the a_ending list. This is done so both the hashing and the
     list creation can be done in one pass over the nodes_str list
   *)
  let a_ending = List.fold_left (fun acc node_str ->
                     let re = Str.regexp {|\([0-9A-Z]+\([A-Z]\)\) = (\([0-9A-Z]+\), \([0-9A-Z]+\))|} in
                     let _ = Str.string_match re node_str 0 in
                     let name = Str.matched_group 1 node_str in
                     let last_letter = Str.matched_group 2 node_str in
                     let left = Str.matched_group 3 node_str in
                     let right = Str.matched_group 4 node_str in
                     begin
                       Hashtbl.add network name (left, right);
                       if last_letter = "A" then name :: acc else acc
                     end) [] nodes_str;
  in
  { network; instructions; a_ending}


(** [navigate map insts end_f start] navigates the [map] using the [insts] instructions
    until the [end_f] function returns true for the current node, starting at [start]
    and returns the number of steps it took to reach the end
 *)
let navigate (map : map) (insts : char list) (end_f : string -> bool) (start : string) : int =
  let rec loop (i : char list) (steps : int) (current : string) : int =
    if end_f current then steps else 
      if List.is_empty i then loop insts steps current else
        begin
        let lr = List.hd i in
        let next_call = loop (List.tl i) (steps + 1) in
        let (left, right) = Hashtbl.find map.network current in
        match lr with | 'L' -> next_call left | 'R' -> next_call right | _ -> failwith "bad instruction"
        end
  in
  loop insts 0 start

(** [lcm a b] returns the least common multiple of [a] and [b] *)
let lcm (a : int) (b : int) : int =
  let rec gcd (a : int) (b : int) : int =
    if b = 0 then a else gcd b (a mod b)
  in
  (a * b) / (gcd a b)

class t =
  object (_)
    inherit Day_intf.t 8

    (** [part1 i] returns the answer to part 1 of the day 8 problem applied to input [i].
        This is the number of steps it takes to navigate the network from AAA to ZZZ
     *)
    method part1 (i : string) : string =
      let map = parse_input i in
      let insts = String.to_seq map.instructions |> List.of_seq in
      let steps = navigate map insts ((=) "ZZZ") "AAA" in
      string_of_int steps

    (** [part2 i] returns the answer to part 2 of the day 8 problem applied to input [i].
        This is the number of steps it takes to navigate the network from AAA to ZZZ
        for all starting nodes at the same time
     *)
    method part2 (i : string) : string =
      let map = parse_input i in
      let insts = String.to_seq map.instructions |> List.of_seq in
      let all_steps = List.map (fun s -> navigate map insts (fun x -> String.get x 2 = 'Z') s) map.a_ending in
      let steps = List.fold_left lcm 1 all_steps in
      string_of_int steps
  end



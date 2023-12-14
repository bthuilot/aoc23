(** Day 12: Hot Springs

    https://adventofcode.com/2023/day/12

    This was not a fun one, it took me till the next
    night to solve. I spent the whole first day just trying
    to see what was the best way to approach the problem. After
    a lot of thinking, the next day I started programming
    what I thought was the best approach. I knew this was dynamic
    programming so I knew it was going to be some sort of function like
    f(springs, size) = f(cdr springs, size) + f(springs, cdr size).

    I eventually came to use a "sliding window" approach where I
    for an input of '##.##' and \[2; 1\] I would take the first
    chekc putting the 2 block down on the first two blocks, If that
    works I would check the remaining possibilities of the rest of the
    sizes (namely \[1\]) on the rest of the input not blocked by the
    2 (namely \[.##\]) by recursively calling the function.
    I called this "point" in the code.

    Then I would then also check the possiblity of not putting the 2
    block down and by recursively calling the function with the
    remaining sizes and the rest of the input (minus one character, since
    in the case we dont put the two, it becomes a '.').
    I called this "recur" in the code.

    I then just added some edge case logic (like knowing
    we cant recur if the first character is a '#', since we'll
    be not putting a block down on the first character, and
    skipping the first character in the recur case if we end on
    a block, etc.) and the code worked.

    Next part 2 came, and I was stuck again. I knew my solution was
    right, but when I ran it took 10 minutes to calculate the first 5 rows.
    After printf debugging to optimize stuff (since i was just gonna try to
    bruteforce) I realized that the problem was that I was recalculating
    the same thing over and over again. And thats when I had major flashbacks
    to my algorithms class and realized I needed to just cache results. I did
    a hash table first and it worked. After I got my gold star I looked at the subreddit
    and realized that I could have just used a matrix, since for both lists as inputs,
    two inputs of the same size will always have the same result, since we only remove
    from the front of the list. So I changed it to a matrix and it worked.
    nice cleeeean code.
 *)


(** [spring_row] is a list of characters representing a row of springs.
    '#' represents a damaged sprint, '.' represents an operational spring,
    and '?' represent a space that can be either damaged or operational.
 *)
type spring_row = char list

(** [condition_record] is a record representing a row of springs and
    a list of group sizes of broken springs.
 *)
type condition_record = {
    row: spring_row;
    groups: int list;
  }

(** [parse_groups i] parses a string of comma separated integers into a list
    of integers.
 *)
let parse_groups (i : string) : int list =
  let groups = String.split_on_char ',' i in
  List.map int_of_string groups
  
(** [parse_condition_records i] parses a string of rows of springs and
    group sizes into a list of condition records.
 *)
let parse_condition_records (i : string) : condition_record list =
  let rows = String.split_on_char '\n' i in
  let parse_line (l : string) : condition_record =
    let l_split = String.split_on_char ' ' l in
    match l_split with
    | [row; groups] ->
       let chars = String.to_seq row |> List.of_seq in
       {
         row = chars;h
         groups = parse_groups groups;
       }
    | _ -> failwith "Invalid input"
  in
  List.map parse_line rows

(** [find_arrangements r] finds the number of arrangements of springs
    that satisfy the condition record [r].
 *)
let find_arrangements (r: condition_record) : int =
  (* how i didnt not miss dynamic programming *)
  let dyn = Array.make_matrix (List.length r.row + 1) (List.length r.groups + 1) (-1) in
  let rec f' (l: char list) (group_sizes: int list) : int =
    if dyn.(List.length l).(List.length group_sizes) >= 0 then
      dyn.(List.length l).(List.length group_sizes)
    else
      match group_sizes with
      | [] -> if (List.exists ((=) '#') l) then 0 else 1
      | size :: rst ->
         if size > List.length l then 0 else
           let (group, remainder)  = (Utils.take size l, Utils.drop size l) in
           (* point is the number of arrangements if we group using current size *)
           let point = if List.exists ((=) '.') group then 0 else
                         match remainder with
                         | [] -> if List.is_empty rst then 1 else 0
                         | '#' :: _ -> 0
                         | _ :: xs -> f' xs rst in
           (* recur is the number of arrangements if we dont group the current size and move on *)
           let recur = if (List.hd group) = '#' then 0 else f' (List.tl l) group_sizes in
           let res = point + recur in
           dyn.(List.length l).(List.length group_sizes) <- res;
           res
  in f' r.row r.groups

(** [unfold_records amt records] takes a list of condition records and
    unfolds each record into [amt] records, where each record has the same
    row and a group list of length [amt] with the same groups.
 *)
let unfold_records (amt: int) (records: condition_record list) : condition_record list =
  (* Function to flatten a list of lists with a separator *)
  let rec flatten s = function
    | [] -> []
    (* this is def slow but I couldnt find anything better online and I was lazy *)
    | x0 :: x1 :: xs -> (x0 @ [s] @ flatten s (x1 :: xs)) 
    | [x] -> x
  in
  List.map (fun r ->
      {
        row = flatten '?' (List.init amt (fun _ -> r.row));
        groups = List.init amt (fun _ -> r.groups) |> List.concat;
      }
    ) records

 
class t =
  object (_)
    inherit Day_intf.t 12
    
    method part1 (i : string) : string =
      let records = parse_condition_records i in
      List.fold_left (fun acc r ->
          acc + (find_arrangements r)
        ) 0 records |> string_of_int
    
    method part2 (i : string) : string =
      let records = parse_condition_records i in
      let unfolded = unfold_records 5 records in
      List.fold_left (fun acc r ->
          acc + (find_arrangements r)
        ) 0 unfolded |> string_of_int
      
  end



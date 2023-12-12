(** Day 4: Scatrchcard

    http://adventofcode.com/2023/day/4

    This day turned out to be a lot easier than yesterday.
    As with the other days in this year, I first parse the input
    into a type that can be used for both days. In this case a record
    with the winning numbers, my numbers, and the matches between them.

    For part 1, I just fold over the list of cards and add the score of
    each card to the accumulator. The caluclation of the score of a card is
    1 point for the first match and then 2x for each additional match, which
    is equivalent to 2^(n-1) where n is the number of matches.

    For part 2, I fold over the list of cards keeping track of the total
    number of each card and the total number of cards collected.
    Every time a card has 'n' matches the next 'n' cards are duplicated,
    so I iterate through the list of card counts and add the current card count to the next 'n' card counts.
    I also add the current card count to the total number of cards collected.
    The total number of cards collected is the answer.
 *)


(** [scartchcard] represents a scartchcard with winning numbers and my numbers
    and the matches between them.
 *)
type scartchcard = {
    winning_numbers: int list;
    my_numbers: int list;
    matches: int list;
  }

(** [SI] is a set of integers. *)
module SI = Set.Make(Int)

(** [pow base exp] is [base] raised to the power of [exp]. *)
let pow (base : int) (exp : int) : int =
  (* Can you believe this isnt in the stdlib?
     I guess i could have imported Jane Streets' [Base] module but that seems
     like overkill for this one function.
   *)
  let rec pow' (acc : int) (exp : int) : int =
    if exp = 0 then acc
    else pow' (acc * base) (exp - 1)
  in
  pow' 1 exp

(** [parse_card_numbers input] is a list of integers parsed from [input].
    [input] is a string of integers separated by any amount of spaces.
 *)
let parse_card_numbers (input : string) : int list =
  List.map int_of_string (Str.split (Str.regexp "[ ]+") input)

(** [find_matches my winning] is a list of integers that are in both [my] and
    [winning]. [my] and [winning] are sorted lists of integers.
 *)
let rec find_matches (my : int list) (winning : int list) : int list =
  match my, winning with
  | _, [] -> []
  | [], _ -> []
  | m :: myRst, w :: wRst  ->
     if m = w then m :: find_matches myRst wRst
     else if m < w then find_matches myRst winning
     else find_matches my wRst

(** [parse_scartchcards input] is a list of scartchcards parsed from [input].
    [input] is a string of scartchcards separated by newlines. Each scartchcard
    is a string of the form "Card [n]: [winning numbers] | [my numbers]" where
    [n] is the card number, [winning numbers] is a list of integers separated
    by any amount of spaces, and [my numbers] is a list of integers separated
    by any amount of spaces.
 *)
let parse_scartchcards (input : string) : scartchcard list =
  let lines = String.split_on_char '\n' input in
  let r = Str.regexp {|Card[ ]+[0-9]+:[ ]+\([0-9 ]+\)[ ]+|[ ]+\([0-9 ]+\)|} in
  let parsed_cards =
    List.map (fun line ->
        let _ = Str.string_match r line 0 in
        let my_numbers_str = Str.matched_group 1 line in
        let winning_numbers_str = Str.matched_group 2 line in
        let winning = parse_card_numbers winning_numbers_str in
        let my = parse_card_numbers my_numbers_str in
        { winning_numbers = winning;
          my_numbers = my;
          matches = find_matches (List.sort compare my) (List.sort compare winning)
        }
      ) lines in
  parsed_cards


(** [card_acc] is an accumulator for the part 2 solution. It keeps track of the
    number of each card and the total number of cards that have been recorded
 *)
type card_acc = {
    counts: int list;
    total: int;
  }


class t =
  object (_)
    inherit Day_intf.t 4
    method part1 (i : string) : string =
      let cards = parse_scartchcards i in
      List.fold_left (fun acc card ->
          let amt = List.length card.matches in
          if amt = 0 then acc
          else (pow 2 (amt- 1)) + acc
        ) 0 cards |> string_of_int

    
    method part2 (i: string) : string =
      let cards = parse_scartchcards i in
      let counts =
        List.fold_left (fun acc card ->
            let card_amt = List.hd acc.counts in
            {
              total = acc.total + card_amt;
              counts = List.tl acc.counts |>
                         List.mapi (fun i c ->
                             if i < List.length card.matches then c + card_amt
                             else c
                           );
            }
          ) {
            counts = List.init (List.length cards) (fun _ -> 1);
            total = 0;
          } cards
      in
      counts.total |> string_of_int
  end



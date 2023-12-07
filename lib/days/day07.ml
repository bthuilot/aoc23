(** Day 7: Camel Cards

    https://adventofcode.com/2023/day/7

    Another pretty easy one once I figured out my strategy.
    The way I handled this was parse all the hands and bids into
    a list, enriching the data with the suit rank. Then I sorted
    the list by suit rank, and then by card value. Then I just
    multiplied the bid by the position in the list (+1) and summed
    them all up.

    For parsing suits, I created a Hashtable of the card counts,
    and then could figure out the suit by the number of distinct
    cards and the max count of any card.

    Handling for jokers then just involved removing jokers from
    the Hashtable and adding their count to the max count of the
    most common card.
 *)


(** [camel_card] represents a camel card. *)
type camel_card = {
    hand: char list;
    bid: int;
    suit : int;
  }

(** [calc_suit_rank distinct max_count] calculates the suit rank
    for a hand of cards. [distinct] is the number of distinct
    cards in the hand, and [max_count] is the max count of any
    card in the hand.
 *)
let calc_suit_rank (distinct : int) (max_count : int) : int =
  match (distinct, max_count) with
    | (1, 5) -> 7 (* 5 of a kind *)
    | (2, 4) -> 6 (* 4 of a kind *)
    | (2, 3) -> 5 (* full house *)
    | (3, 3) -> 4 (* 3 of a kind *)
    | (3, 2) -> 3 (* 2 pair *)
    | (4, 2) -> 2 (* 1 pair *)
    | (5, 1) -> 1 (* high card *)
    | _ -> 0

(** [suit_rank jokers hand] calculates the suit rank for a hand
    of cards. [jokers] is whether or not the player is playing
    with jokers, and [hand] is the hand of cards.
 *)
let suit_rank (jokers: bool) (hand : char list) : int =
  let ht = Hashtbl.create 5 in
  List.iter (fun c ->
      let count = Option.value (Hashtbl.find_opt ht c) ~default:0
      in Hashtbl.replace ht c (count + 1)
    ) hand;
  let js = if jokers then
             let js = Option.value (Hashtbl.find_opt ht 'J') ~default:0 in
             (Hashtbl.remove ht 'J'; js)
           else 0 in
  let len = Hashtbl.length ht in
  let m = Hashtbl.fold (fun _ v acc -> max v acc) ht 0 in
  calc_suit_rank (max len 1) (m + js)

(** [parse_camel_cards jokers i] parses the camel cards from
    the input string [i]. [jokers] is whether or not the player
    is playing with jokers.
 *)
let parse_camel_cards (jokers: bool) (i : string) : camel_card list =
  let lines = String.split_on_char '\n' i in
  List.map (fun line ->
      let parts = String.split_on_char ' ' line in
      let bid = int_of_string (List.nth parts 1) in
      let hand_str = List.hd parts in
      let hand = String.to_seq hand_str |> List.of_seq in
        {
          hand = hand;
          bid = bid;
          suit = suit_rank jokers hand;
        }
    ) lines

(** [card_value jokers c] calculates the value of a card. [jokers]
    is whether or not the player is playing with jokers, and [c]
    is the card.
 *)
let card_value (jokers: bool) (c : char) : int =
  match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> if jokers then 1 else 11
    | 'T' -> 10
    | _ -> Char.code c - Char.code '0'

(** [calculate_winnings jokers i] calculates the winnings for
    the camel cards in [i]. winnings are calculated by multiplying
    the bid by the position in the sorted list (+1). [i] is the
    input string, and [jokers] is whether or not the player is
    playing with jokers.
 *)
let calculate_winnings (jokers: bool) (i: string) : int =
  let cards = parse_camel_cards jokers i in
  let compare_card = card_value jokers in
  let sorted =
    List.sort (fun a b ->
        if a.suit = b.suit
        then
          List.fold_left2 (fun acc a' b' ->
              if acc = 0 then (compare_card a') - (compare_card b')  else acc
            ) 0 a.hand b.hand
        else
          a.suit - b.suit
      ) cards in  
  List.mapi (fun i card ->
      (i + 1) * card.bid
    ) sorted |> List.fold_left (+) 0 


class t =
  object (_)
    inherit Day_intf.t 7
    
    method part1 (i : string) : string = calculate_winnings false i |> string_of_int
      
    method part2 (i : string) : string = calculate_winnings true i |> string_of_int
  end




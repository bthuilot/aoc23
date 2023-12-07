(** Day 7: Camel Cards

    https://adventofcode.com/2023/day/7
    
 *)

type camel_card = {
    hand: char list;
    bid: int;
    suit : int;
  }


let calc_suit_rank (hand : char list) : int =
  let ht = Hashtbl.create 5 in
  List.iter (fun c ->
      let count = match Hashtbl.find_opt ht c with
        | None -> 0
        | Some x -> x
      in
      Hashtbl.replace ht c (count + 1)
    ) hand;
  let len = Hashtbl.length ht in
  let max = Hashtbl.fold (fun _ v acc -> max v acc) ht 0 in
  match (len, max) with
    | (1, 5) -> 7 (* 5 of a kind *)
    | (2, 4) -> 6 (* 4 of a kind *)
    | (2, 3) -> 5 (* full house *)
    | (3, 3) -> 4 (* 3 of a kind *)
    | (3, 2) -> 3 (* 2 pair *)
    | (4, 2) -> 2 (* 1 pair *)
    | (5, 1) -> 1 (* high card *)
    | _ -> 0


let parse_camel_cards (i : string) : camel_card list =
  let lines = String.split_on_char '\n' i in
  List.map (fun line ->
      let parts = String.split_on_char ' ' line in
      let bid = int_of_string (List.nth parts 1) in
      let hand_str = List.hd parts in
      let hand = List.init (String.length hand_str) (String.get hand_str) in
        {
          hand = hand;
          bid = bid;
          suit = calc_suit_rank hand;
        }
    ) lines

let card_value (c : char) : int =
  match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | _ -> int_of_string (String.make 1 c) 

let compare_hands (a : char list) (b : char list) : int =
  List.fold_left2 (fun acc a' b' ->
      if acc = 0 then (card_value a') - (card_value b')  else acc
    ) 0 a b

class t =
  object (_)
    inherit Day_intf.t 7
    method part1 (i : string) : string =
      let cards = parse_camel_cards i in
      let sorted =
        List.sort (fun a b ->
            if a.suit = b.suit then compare_hands a.hand b.hand else a.suit - b.suit
          ) cards in
      List.mapi (fun i card ->
          (i + 1) * card.bid
        ) sorted |> List.fold_left (+) 0 |> string_of_int
    method part2 (_ : string) : string = "Not Implemented"
  end




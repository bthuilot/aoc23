
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_symbol = function
  | '0' .. '9' | '.' -> false
  | _ -> true

type symbol_location = (int * int)

type symbols = (symbol_location, char) Hashtbl.t

let parse_symbols (l : string list) : ((int * int), char) Hashtbl.t =
  let h = Hashtbl.create (10 * List.length l) in
  List.iteri (fun row s ->
      String.iteri (fun col c ->
          if is_symbol c then
          Hashtbl.add h (row, col) c
        ) s
    ) l;
  h

let bordering_cords (row : int) (col : int) : (int * int) list =
  [(row - 1, col - 1); (row - 1, col); (row - 1, col + 1);
   (row, col - 1); (row, col + 1);
   (row + 1, col - 1); (row + 1, col); (row + 1, col + 1)]

let is_symbol_adjancet (symbls : symbols) (row : int) (col : int) : bool =
  bordering_cords row col
  |> List.exists (fun (row, col) -> Hashtbl.mem symbls (row, col))

type col_acc = {seen_symbol: bool; cur_num: char list; col: int; parsed_nums: int list}
type row_acc = {parsed_nums: int list; row: int}

let char_list_to_int (l : char list) : int =
  if List.length l = 0 then 0
  else
  l |> List.rev |> List.to_seq |> String.of_seq |> int_of_string

let parse_part_numbers (symbls : symbols) (l : string list) : int list =
  let row_res = List.fold_left (fun row_acc s ->
      let col_res = String.fold_left (fun acc c ->
          if is_digit c then
            {acc with seen_symbol = acc.seen_symbol || is_symbol_adjancet symbls row_acc.row acc.col;
                      cur_num = c :: acc.cur_num;
                      col = acc.col + 1
            }
          else
            if List.length acc.cur_num > 0 then
              if acc.seen_symbol then
                {parsed_nums = (char_list_to_int acc.cur_num) :: acc.parsed_nums;
                 cur_num = [];
                 seen_symbol = false;
                 col = acc.col + 1
                }
              else
                {acc with seen_symbol = false; cur_num = []; col = acc.col + 1}
            else
              {acc with seen_symbol = false; col = acc.col + 1}
        ) {
                seen_symbol = false;
                cur_num = [];
                col = 0;
                parsed_nums = []
                      } s; in
      let last_num = if col_res.seen_symbol then
                       char_list_to_int col_res.cur_num
                     else
                       0 in
      {
        parsed_nums = List.append row_acc.parsed_nums (last_num :: col_res.parsed_nums);
        row = row_acc.row + 1
      }
                  ) {row = 0; parsed_nums = []} l;
  in
        row_res.parsed_nums


class t =
  object (_)
    inherit Day_intf.t 3
    method part1 (i : string) : string =
      let lines = String.split_on_char '\n' i in
      let symbols = parse_symbols lines in
      let part_numbers = parse_part_numbers symbols lines in
      part_numbers |> List.fold_left (+) 0 |> string_of_int
      
      (* iter_lines [] lines |> List.fold_left (fun acc n -> int_of_string n |> ((+) acc)) 0 |> string_of_int *)
    method part2 (_ : string) : string = "Not Implemented"
  end



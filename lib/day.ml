open Core

class virtual t (num: int) =
        object (self)
          method virtual part1: string -> string
          method virtual part2: string -> string
          method get_num = num

          method run =
            let input = self#read_input in
            let part1 = self#part1 input in
            let part2 = self#part2 input in
            Printf.printf "Day %d:\n" num;
            Printf.printf "  part 1: %s" part1;
            Printf.printf "  part 2: %s" part2;
            Printf.printf "\n"
          

          method read_input = 
            In_channel.read_all (Printf.sprintf "inputs/%02d.txt" num)
        end;;


class t =
  object (_)
    inherit Day.t 0
    method part1 (input : string) : string = input
    method part2 (input : string) : string = String.uppercase_ascii input
  end



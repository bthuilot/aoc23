
type power = On | Off

type pulse = High | Low

type module_type = 
  | FlipFlop of power ref
  | Conjuction of ((string, pulse) Hashtbl.t)

type com_module = {
    name: string;
    module_type: module_type;
    destinations: string list;
  }
    

type machine = {
    broadcaster: string list;
    modules: (string, com_module) Hashtbl.t;
  }

let parse_destinations (dests: string) : string list =
  Str.split (Str.regexp ",[ ]*") dests

let parse_input (input : string) : machine =
  let lines = String.split_on_char '\n' input in
  let tbl  = Hashtbl.create (List.length lines) in
  let broadcaster = List.fold_left
                      (fun acc line ->
                        let is_broadcast = Str.string_match (Str.regexp {|^broadcaster -> \([,a-z ]+\)$|}) line 0 in
                        if is_broadcast then Str.matched_group 1 line |> parse_destinations |> Option.some 
                        else
                          let _ = Str.string_match (Str.regexp {|^\([%&]\)\([a-z]+\) -> \([a-z, ]+\)$|}) line 0 in
                          let module_type = match Str.matched_group 1 line with
                            | "%" -> FlipFlop (ref Off)
                            | "&" -> Conjuction (Hashtbl.create (List.length lines))
                            | _ -> failwith "Invalid module type"
                          in
                          let name = Str.matched_group 2 line in
                          let destinations = Str.matched_group 3 line |> parse_destinations in
                          Hashtbl.add tbl name {name; module_type; destinations};
                          acc
                      ) None lines in
  Hashtbl.iter (fun name v ->
      match v.module_type with
      | FlipFlop _ -> ()
      | Conjuction pulse ->
         Hashtbl.iter (fun source {destinations; _} ->
             if List.exists (fun dest -> dest = name) destinations then
               Hashtbl.add pulse source Low else ()
          ) tbl;
    ) tbl;
  match broadcaster with
  | None -> failwith "No broadcaster found"
  | Some b -> {broadcaster = b; modules = tbl}
  

let press_button ?(high = 0) ?(low = 0) (machine: machine) : (int * int * bool) =
  let q = Queue.create () in
  let low_rx = ref 0 in
  let rec press_button' (highs: int) (lows: int) : (int* int) =
    if Queue.is_empty q then (highs, lows)
    else
      let (name, incoming_pulse, previous)  = Queue.pop q in
      match Hashtbl.find_opt machine.modules name with
      | None -> press_button' (highs) (lows)
      | Some module' ->
         let () = if name = "rx" && incoming_pulse = Low then low_rx := !low_rx + 1 else () in
         let next_pulse: pulse option = match module'.module_type with
           | FlipFlop power ->
              begin
                if incoming_pulse = High then None
                else
                  let p = !power in
                  let () = power := if p = On then Off else On in
                  match p with
                  | On -> Some Low
                  | Off -> Some High
              end
           | Conjuction pulse ->
              let () = Hashtbl.replace pulse previous incoming_pulse in
              let all_flipped = Hashtbl.fold (fun _ v acc -> acc && v = High) pulse true in

              if all_flipped then Some Low else Some High
         in
         match next_pulse with
         | None -> press_button' highs lows
         | Some np ->
            let () = List.iter (fun dest -> Queue.push (dest, np, name) q) module'.destinations in
            let amt = List.length module'.destinations in
            let (highs', lows') = match np with
              | High -> (highs + amt, lows)
              | Low -> (highs, lows + amt)
            in
            press_button' highs' lows'
  in
  let () = List.iter (fun dest -> Queue.push (dest, Low, "broadcaster") q) machine.broadcaster in
  let (h, l) = press_button' high (low + List.length machine.broadcaster + 1) in
  h, l, !low_rx = 1


class t =
  object (_)
    inherit Day_intf.t 20
    method part1 (i : string) : string =
      let machine = parse_input i in
      let (h,l) = List.fold_left (fun (h, l) _ ->
          let (h', l', _) = press_button ~high:h ~low:l machine in
          h', l'
        ) (0, 0) (List.init 1000 (fun _ -> ())) in
      string_of_int (h * l)
    method part2 (i : string) : string =
      let machine = parse_input i in
      let count = ref 0 in
      let found = ref false in
      let (h, l) = ref 0, ref 0 in
      while not !found do
        let () = count := !count + 1 in
        let (h',l', rx) = press_button ~high:!h ~low:!l machine in
        let () = found := rx in
        h := h'; l := l'
      done;
      string_of_int !count
  end



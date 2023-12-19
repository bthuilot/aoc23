type 'a t = (int * 'a) list

let empty = []

let rec enqueue queue priority element =
  match queue with
  | [] -> [(priority, element)]
  | (p, e) :: rest ->
    if priority < p then (priority, element) :: queue
    else (p, e) :: enqueue rest priority element

let length = List.length

let dequeue = function
  | [] -> None
  | (_, head) :: rst -> Some (head, rst)

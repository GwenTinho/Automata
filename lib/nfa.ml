type 'a nfa = {
  initial : 'a list;
  final : 'a -> bool;
  transitions : char -> (('a * 'a) list)
}



let rec directImage  (r : ('a * 'a) list) = function
  | [] -> []
  | h :: t -> (List.filter_map (fun (p,q) -> if p == h then Some q else None) r) @ (directImage r t)


let iterate (n : 'a nfa) w =
  let rec aux acc = function
  | [] -> acc
  | a :: t ->
    let newAcc = directImage (n.transitions a) acc  in aux newAcc t in aux n.initial w

let accepts n w = List.exists n.final (iterate n w)


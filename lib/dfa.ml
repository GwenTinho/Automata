type 'a dfa = {
  initial : 'a;
  final : 'a -> bool;
  transitions : char -> 'a -> 'a
}

let iterate (n : 'a dfa) w = List.fold_left (fun st a -> n.transitions a st) n.initial w

let accepts (n : 'a dfa) w = n.final (iterate n w)


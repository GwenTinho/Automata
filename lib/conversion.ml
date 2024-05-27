open Nfa
open Dfa

let convert (n : 'a Nfa.nfa) : ('a list) Dfa.dfa =
  {
    initial = n.initial;
    final = List.exists n.final;
    transitions = fun a -> Nfa.directImage (n.transitions a)
  }

(*We already ran reach here in theory*)

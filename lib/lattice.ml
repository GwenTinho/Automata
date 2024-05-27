
(* Function to find elements covered by a given element *)

let covers poset x =
    List.filter (fun (a, _) -> a = x) poset |> List.map snd

(* Function to find join-irreducible elements *)
let find_join_irreducible poset elements =
  let is_join_irreducible x =
    let covered_by = covers poset x in
    List.length covered_by = 1
  in
  List.filter is_join_irreducible elements

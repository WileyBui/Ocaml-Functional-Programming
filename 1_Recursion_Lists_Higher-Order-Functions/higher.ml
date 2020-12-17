(* PART 2 *)
let all_odds (lst: int list) : int list =
  List.filter (fun x -> if x mod 2 = 0 then false else true) lst

let decrement_all (lst: int list) : int list =
  List.map (fun x -> x - 1) lst

let min_fold (lst: int list) : int =
  match lst with
  | [] -> raise (Failure "The list must have > 0 elements.")
  | head::_ -> List.fold_left (fun x y -> if x > y then y else x) head lst

let sum_prod (lst: int list) : int * int =
  List.fold_left (fun (x, y) z -> (z + x, z * y)) (0, 1) lst

let partition_left (f: 'a -> bool) (lst: 'a list) : ('a list * 'a list) =
  List.fold_left (fun (x, y) z -> if f z then (x @ [z], y) else (x, y @ [z])) ([], []) lst

let partition_right (f: 'a -> bool) (lst: 'a list) : ('a list * 'a list) =
  List.fold_right (fun z (x, y) -> if f z then (z::x, y) else (x, z::y)) lst ([], [])


(*  
  No difficulties faced.
  I made Listfold_left as well as List.fold_right, just switched the order around:

  let map_as_fold (f: 'a -> 'b) (lst: 'a list) : 'b list =
    List.fold_right (fun y x -> f y::x) lst []
*)
let map_as_fold (f: 'a -> 'b) (lst: 'a list) : 'b list =
  List.fold_left (fun x y -> x @ [f y]) [] lst
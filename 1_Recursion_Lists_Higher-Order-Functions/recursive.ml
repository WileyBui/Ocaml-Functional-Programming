(* PART 1 *)
let odd number = 
  if number mod 2 = 0 then false else true

let rec euclid (x: int) (y: int) : int = 
  if x = y then x
  else if x < y then euclid x (y-x)
  else euclid (x-y) y

let frac_simplify (x, y) : (int * int) =
  let my_euclid = euclid x y
  in (x / my_euclid, y / my_euclid)

let rec min_list (lst: int list) : int =
  match lst with
  | [] -> raise (Failure "The list must have > 0 elements.")
  | x::[] -> x
  | x::rest -> if x < min_list rest then x else min_list rest

let rec drop (count: int) (lst: 'a list) : 'a list =
  if count = 0 then lst
  else 
    match lst with
    | [] -> []
    | _::rest -> drop (count-1) rest
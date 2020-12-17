# Homework 4: Reasoning about Correctness.

by Wiley Bui

## Problem 1
``` ocaml
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys

(*
  show by induction that
    prod (l1 @ l2) = prod l1 * prod l2
*)
```

1. the base case that you prove,
  ```
    when l1 is an empty list: 
      prod ([] @ l2) = prod [] * prod l2
  ```  

2. the inductive case that your prove,
  ```
    prod (y::ys @ l2) = prod (y::ys) * prod l2
  ```

3. and the inductive hypothesis used in your proof.
  ```
    prod (ys @ l2) = prod (ys) * prod l2
  ```

4. the principle of induction that you are using in your proof. This is tied to the type of the values over which you are doing induction.
  ```
    P(l1) holds if
      P([]) holds
    if P(l1') and P(l1'') hold then
      P(y::ys) holds
  ```

5. proof of the base case
  ```
    prod ([] @ l2)
    = prod (l2)
        by the definition of append
    = prod [] * prod l2
        by the empty list identity and by the definition of prod
  ```

6. proof of the inductive case
  ```
    prod (y::ys @ l2)
    = prod (y :: (ys @ l2))
        by definition of @
    = y * prod (ys @ l2)
        by definition of prod
    = y * prod (ys) * prod l2
        by definition of induction hypothesis
    = prod (y::ys) * prod l2
        by definition of prod
  ```


## Problem 2
``` ocaml
let rec sum = function
  | [] -> 0
  | y::ys -> y + sum ys

let rec length = function
  | [] -> 0
  | y::ys -> 1 + length ys

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys

(*
  Using the definitions above, show by induction that
    sum (inc_all l) = length l + sum l
*)
```

1. the base case that you prove,
  ```
    when l is an empty list:
      sum (inc_all []) = length [] + sum []
  ```
  
2. the inductive case that your prove,
  ```
    sum (inc_all (y::ys)) = length (y::ys) + sum (y::ys)
  ```
  
3. and the inductive hypothesis used in your proof.
  ```
    sum (inc_all ys) = length ys + sum ys
  ```
  
4. the principle of induction that you are using in your proof. This is tied to the type of the values over which you are doing induction.
  ```
    P(l) holds if
      P([]) holds
    if P(l1) and P(l2) hold then
      P(y::ys) holds
  ```
  
5. proof of the base case
  ```
    sum (inc_all [])
    = sum ([])
        by the definition of inc_all
    = 0
        by the definition of sum
    = 0 + 0
        by arithmetics
    = 0 + sum []
        by the definition of sum
    = length [] + sum []
        by the definition of length
  ```
  
6. proof of the inductive case
  ```
    sum (inc_all (y::ys))
    = sum ((y + 1) :: inc_all ys)
        by the definition of inc_all
    = (y+1) + sum (inc_all ys)
        by the definition of sum
    = (y+1) + length ys + sum ys
        by definition of induction hypothesis
    = (1 + length ys) + (y + sum ys)
        by organization & arithmetics
    = length (y::ys) + (y + sum ys)
        by the definition of length
    = length (y::ys) + sum(y::ys)
        by the definition of sum
  ```
  

## Problem 3
```ocaml
let rec map f l = match l with
  | [] -> []
  | y::ys -> f y :: map f ys

let inc x = x + 1

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys

(*
  show by induction that
    map inc l = inc_all l
*)
```

1. the base case that you prove,
  ```
    when l is an empty list: 
      map inc [] = inc_all []
  ```
  
2. the inductive case that your prove,
  ```
    map inc (y::ys) = inc_all (y::ys)
  ```
  
3. and the inductive hypothesis used in your proof.
  ```
    map inc ys = inc_all ys
  ```
  
4. the principle of induction that you are using in your proof. This is tied to the type of the values over which you are doing induction.
  ```
    P(l) holds if 
      P([]]) holds
    if P(l1) and P(l2) hold then
      P(y::ys) holds
  ```
  
5. proof of the base case
  ```
    map inc [] = inc_all []

    map inc []
    = map ([])
        by the definition of inc
    = []
        by the definition of map
    = inc_all []
        by the definition of inc_all
  ```
  
6. proof of the inductive case
  ```
    map inc (y::ys)
    = inc y :: map inc ys
        by the definition of map
    = inc y :: inc_all ys
        by the inductive hypothesis
    = (y+1) :: inc_all ys
        by the definition of inc
    = inc_all (y::ys)
        by the definition of inc_all
  ```
  
## Problem 4
``` ocaml
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

let rec to_list (t: 'a tree) : 'a list = match t with
  | Empty -> []
  | Node (v, tl, tr) -> to_list tl @ [v] @ to_list tr

let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys

let rec product (my_tree: int tree) : int =
  match my_tree with
  | Empty -> 1
  | Node (v, tl, tr) -> v * product tl * product tr

(* 
  show by induction that
    prod (to_list t) = product t
*)
```

1. the base case that you prove,
  ```
    when t is Empty:
      prod (to_list Empty) = product Empty
  ```
  
2. the inductive case that your prove,
  ```
    prod (to_list Node(v, t1, t2)) = product (Node (v, t1, t2))
  ```
  
3. and the inductive hypothesis used in your proof.
  ```
    prod (to_list t*) = product t*,
      where * is 1 in t1, 2 in t2, ...
  ```
  
4. the principle of induction that you are using in your proof. This is tied to the type of the values over which you are doing induction.
  ```
    P(t) holds if
      P(Empty) holds
    if P(t') and P(t'') hold then
      P(Node (v, t1, t2)) holds
  ```
  
5. proof of the base case
  ```
    prod (to_list Empty)
    = prod ([])
        by the definition of to_list
    = 1
        by the definition of prod
    = product Empty
        by the definition of product
  ```
  
6. proof of the inductive case
  ```
    prod (to_list Node(v, t1, t2))
    = prod (to_list t1 @ [v] @ to_list t2)
        by the definition of to_list
    = v * prod (to_list t1) * prod (to_list t2)
        by the definition of prod
    = v * product t1 * product t2
        by the induction hypothesis
    = product (Node (v, t1, t2))
        by the definition of product
  ```
  

## Question 5
```ocaml
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
  match t with
  | Empty -> b
  | Node (v, t1, t2) -> f v (reduce t1 b f) (reduce t2 b f)

let rec size (my_tree: 'a tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) -> 1 + size t1 + size t2

let size_r (tree: 'a tree) : int =
  reduce tree 0 (fun v t1 t2 -> 1 + t1 + t2) 

(*
  Show by induction that
    size t = size_r t
*)
```

1. the base case that you prove,
  ```
    when t is Empty:
      size Empty = size_r Empty
  ```
  
2. the inductive case that your prove,
  ```
    size Node(v, t1, t2) = size_r Node(v, t1, t2)
  ```
  
3. and the inductive hypothesis used in your proof.
  ```
    size t* = size_r t*,
      where * is 1 in t1, 2 in t2, ...
  ```
  
4. the principle of induction that you are using in your proof. This is tied to the type of the values over which you are doing induction.
  ```
    P(t) holds if
      P(Empty) holds
    if P(t1) and P(t2) hold then
      P(Node (v, tl, tr)) holds
  ```
  
5. proof of the base case
  ```
    size Empty = size_r Empty
    = 0
        by the definition of size
    = reduce Empty 0 (fun v t1 t2 -> 1 + t1 + t2) 
        by the definition of reduce
    = sum_r Empty
        by the definition of sum_r
  ```
  
6. proof of the inductive case
  ```
    size Node(v, t1, t2)
    = 1 + size t1 + size t2
        by the definition of size
    = 1 + size_r t1 + size_r t2
        by the induction hypothesis
    = 1 + t1 + t2
        by the definition of reduce
    = size_r Node(v, t1, t2)
        by the definition of size_r
  ```
  
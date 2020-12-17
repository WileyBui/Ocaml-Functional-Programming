type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* The following int_tree, ints_tree, str_tree, and strs_tree
    are for for the test cases. *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

let int_tree_test_2 : int tree =
  Node (5,
        Node (3,
              Node (1,
                    Empty,
                    Node (6,
                          Empty, 
                          Empty)),
              Empty), 
        Node (30, Empty, 
              Node (12, 
                    Node (10, Empty, Empty),
                    Empty)) 
        )

let int_tree_test_3 : int tree =
  Node (10, 
        Node (5, Empty, Empty), 
        Node (15, Empty, 
              Node (25, Empty, Empty)))

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )

let str_tree_test_2 : string tree = 
  Node ("test ", 
        Node ("This ", 
              Empty,
              Node ("great ", 
                    Node ("a ", 
                          Node ("is ", Empty, Empty),
                          Empty), 
                    Empty)
              ), 
        Node ("freaking ", 
              Node ("to ", Empty, Empty), 
              Node ("use!", Empty, Empty)) 
        )

let str_tree_test_3 : string tree =
  Node ("are ", 
        Node ("How ", Empty, Empty), 
        Node ("you ", Empty, 
              Node ("doing?", Empty, Empty)))

let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let ints_tree_test_2 : int list tree =
  Node ([1;5;4;3],
        Node ([5;3;2],
              Node ([2;3;4;2],
                    Empty,
                    Node ([9;4],
                          Empty, 
                          Empty)),
              Empty), 
        Node ([5;4;3;2], Empty, 
              Node ([5], 
                    Node ([10; 5], Empty, Empty),
                    Empty)) 
        )

let ints_tree_test_3 : int list tree =
  Node ([10;3;2], 
        Node ([5;3;2;4], Empty, Empty), 
        Node ([15;4;5;12], Empty, 
              Node ([5;45;2], Empty, Empty)))

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       ) 

let strs_tree_test_2 : string list tree = 
  Node (["By "; "how "], 
        Node (["How "; "do "; "you "], 
              Empty,
              Node (["sick? "], 
                    Node (["vampire "; "is "], 
                          Node (["tell "; "if "; "a "], Empty, Empty),
                          Empty), 
                    Empty)
              ), 
        Node (["is "], 
              Node (["much "; "he "], Empty, Empty), 
              Node (["coffin"; "!"; "!"], Empty, Empty)) 
        )

let strs_tree_test_3 : string list tree =
  Node (["Who's"; "there? "], 
        Node (["Knock "; "Knock. "], Empty, Empty), 
        Node (["Tank. "], Empty, 
              Node (["Tank "; "who? "], Empty, 
                    Node (["You're"; " welcome!"], Empty, Empty))))

(* This function combines all values from a tree into a single value. *)
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
  match t with
  | Empty -> b
  | Node (v, t1, t2) -> f v (reduce t1 b f) (reduce t2 b f)

(* This function counts the number of values stored in a tree. *)
let size (tree: 'a tree) : int =
  reduce tree 0 (fun v t1 t2 -> 1 + t1 + t2) 

(* This function adds together all the int values stored in an int tree. *)
let sum (tree: int tree) : int =
  reduce tree 0 (fun v t1 t2 -> v + t1 + t2)  

(* This function multiplies together all the int values stored in an int tree. *)
let product (tree: int tree) : int =
  reduce tree 1 (fun v t1 t2 -> v * t1 * t2)   
  
(* This function concatenates all the strings in a string tree. *)
let concat (tree: string tree) : string =
  reduce tree "" (fun v t1 t2 -> t1 ^ v ^ t2) 

(* This function determines the total number of characters stored in the strings in a string tree. *)
let charcount (tree: string tree) : int =
  reduce tree 0 (fun v t1 t2 -> String.length v + t1 + t2) 

(* This function counts the number of elements in the lists in the nodes in the tree. *)
let list_tree_size (trees: 'a list tree) : int =
  reduce trees 0 (fun lst t1 t2 -> List.length lst + t1 + t2) 

(* This function adds up all the number of elements in the lists in the nodes in the int list tree. *)
let list_tree_sum (trees: int list tree) : int =
  reduce trees 0 (fun lst t1 t2 -> 
    List.fold_left (fun a b -> a + b) 0 lst + t1 + t2)
    
(* This function multiplies up all the number of elements in the lists in the nodes in the int list tree. *)
let list_tree_product (trees: int list tree) : int =
  reduce trees 1 (fun lst t1 t2 -> 
    List.fold_left (fun a b -> a * b) 1 lst * t1 * t2)
  
(* This function counts the number of characters in the string lists inside a string list tree. *)   
let list_tree_charcount (trees: string list tree) : int =
  reduce trees 0 (fun lst t1 t2 -> 
    List.fold_left (fun a b -> a + String.length b) 0 lst + t1 + t2)

(* This function concatenates all the strings in the string lists inside a string list tree. *)  
let list_tree_concat (trees: string list tree) : string =
  reduce trees "" (fun lst t1 t2 ->
    t1 ^ List.fold_left (fun a b -> a ^ b) "" lst ^ t2
  )

let () = 
print_string "Testing part 3 ... " ;
try
  assert (size str_tree = 4);
  assert (sum int_tree = 10);
  assert (product int_tree = 24);
  assert (concat str_tree = "I really love OCaml!");
  assert (charcount str_tree = 20);

  assert (size int_tree_test_2 = 7);
  assert (sum int_tree_test_2 = 67);
  assert (product int_tree_test_2 = 324000);
  assert (charcount str_tree_test_2 = 37);
  assert (concat str_tree_test_2 = "This is a great test to freaking use!");

  assert (size int_tree_test_3 = 4);
  assert (sum int_tree_test_3 = 55);
  assert (product int_tree_test_3 = 18750);
  assert (charcount str_tree_test_3 = 18);
  assert (concat str_tree_test_3 = "How are you doing?");

  assert (list_tree_size strs_tree = 11);
  assert (list_tree_sum ints_tree = 45);
  assert (list_tree_product ints_tree = 311040);
  assert (list_tree_charcount strs_tree = 54);
  assert (list_tree_concat strs_tree = 
            "I really do love Ocaml!  It must be your favorite too!");

  assert (list_tree_size strs_tree_test_2 = 17);
  assert (list_tree_sum ints_tree_test_2 = 81);
  assert (list_tree_product ints_tree_test_2 = 93312000000);
  assert (list_tree_charcount strs_tree_test_2 = 64);
  assert (list_tree_concat strs_tree_test_2 = 
            "How do you tell if a vampire is sick? By how much he is coffin!!");
            
  assert (list_tree_size strs_tree_test_3 = 9);
  assert (list_tree_sum ints_tree_test_3 = 117);
  assert (list_tree_product ints_tree_test_3 = 11664000000);
  assert (list_tree_charcount strs_tree_test_3 = 56);
  assert (list_tree_concat strs_tree_test_3 = 
            "Knock Knock. Who'sthere? Tank. Tank who? You're welcome!");
  print_string "tests passed.\n"
with
  Assert_failure (file, line, column) -> 
  let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
              ", column " ^ string_of_int column ^ "\n\n\n\n"
  in print_string msg
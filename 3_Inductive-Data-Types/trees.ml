(* PART 1 *)

(* A tree type declaration. *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
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


(* This function counts the number of values stored in a tree. *)
let rec size (my_tree: 'a tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) -> 1 + size t1 + size t2

(* This function adds together all the int values stored in an int tree. *)
let rec sum (my_tree: int tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) -> v + sum t1 + sum t2
  
(* This function multiplies together all the int values stored in an int tree. *)
let rec product (my_tree: int tree) : int =
  match my_tree with
  | Empty -> 1
  | Node (v, t1, t2) -> v * product t1 * product t2

(* This function determines the total number of characters stored in the strings in a string tree. *)
let rec charcount (my_tree: string tree) =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) -> 
      String.length v + charcount t1 + charcount t2

(* This function concatenates all the strings in a string tree. *)
let rec concat (my_tree: string tree) : string =
  match my_tree with
  | Empty -> ""
  | Node (v, t1, t2) -> concat t1 ^ v ^ concat t2




(* PART 2 - THE CODE BELOW THIS LINE IS SIMILAR AS ABOVE, BUT WITH LISTS AS VALUES IN A TREE *)
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


(* This function counts the number of elements in the lists in the nodes in the tree. *)
let rec list_tree_size (my_tree: 'a list tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) ->
  (* recursively counting up the number of elements in the list *)
  let rec counter lst = 
    match lst with
    | [] -> 0
    | x::xs -> 1 + counter xs
      in 
      counter v + list_tree_size t1 + list_tree_size t2
      
(* This function adds up all the number of elements in the lists in the nodes in the int list tree. *)
let rec list_tree_sum (my_tree: int list tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) ->
  (* recursively adding up all the elements in the list *)
  let rec sum lst = 
    match lst with
      | [] -> 0
      | x::xs -> x + sum xs
      in 
      sum v + list_tree_sum t1 + list_tree_sum t2
      
(* This function multiplies up all the number of elements in the lists in the nodes in the int list tree. *)
let rec list_tree_product (my_tree: int list tree) : int =
  match my_tree with
  | Empty -> 1
  | Node (v, t1, t2) ->
    (* recursively multiplying all the elements in the list *)
    let rec product lst = 
      match lst with
      | [] -> 1
      | x::xs -> x * product xs
    in 
    product v * list_tree_product t1 * list_tree_product t2

(* This function counts the number of characters in the string lists inside a string list tree. *)    
let rec list_tree_charcount (my_tree: string list tree) : int =
  match my_tree with
  | Empty -> 0
  | Node (v, t1, t2) ->
    (* recursively counting all the elements in the list *)
    let rec counter str_lst =
      match str_lst with
      | [] -> 0
      | x::xs -> String.length x + counter xs
    in
    counter v + list_tree_charcount t1 + list_tree_charcount t2

(* This function concatenates all the strings in the string lists inside a string list tree. *)      
let rec list_tree_concat (my_tree: string list tree) : string =
  match my_tree with
  | Empty -> ""
  | Node (v, t1, t2) ->
    (* recursively concatenating all the elements in the list *)
    let rec concat lst = 
      match lst with
      | [] -> ""
      | x::xs -> x ^ concat xs
    in 
    list_tree_concat t1 ^ concat v ^ list_tree_concat t2

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )

       let () = 
        print_string "Testing part 1 ... " ;
        try
          assert (size str_tree = 4);
          assert (sum int_tree = 10);
          assert (product int_tree = 24);
          assert (charcount str_tree = 20);
          assert (concat str_tree = "I really love OCaml!");
          
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

          print_string "tests passed.\n"
        with
          Assert_failure (file, line, column) -> 
          let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                      ", column " ^ string_of_int column ^ "\n\n\n\n"
          in print_string msg

let () = 
  print_string "Testing part 2 ... " ;
  try
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
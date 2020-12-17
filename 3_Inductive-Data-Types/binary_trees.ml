type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

(* This function takes a "compare" function and adds the second argument to the tree passed in as the third argument. *)
let rec insert_by (f: 'a -> 'a -> int) (elem: 'a) (tree: 'a btree) : 'a btree =
  match tree with
  | Nil -> Leaf elem
  | Leaf a -> if f elem a > 0 then Fork (Nil, a, Leaf elem)       (* adds the elem on the right since it's larger than the leaf*)
              else if f elem a < 0 then Fork (Leaf elem, a, Nil)  (* adds the elem on the left since it's larger than the leaf*)
              else Leaf a                                         (* don't do anything since elem is the same as the leaf*)
  | Fork (tree, a, tree2) ->  if compare elem a > 0 then Fork (tree, a, insert_by compare elem tree2)
                              else if compare elem a < 0 then Fork (insert_by compare elem tree, a, tree2)
                              else Fork (tree, a, tree2)


(* This function adds the elements from the list to a tree by calling the insert_by function. *)
let from_list (f: 'a -> 'a -> int) (lst: 'a list) : 'a btree =
  match lst with
  | [] -> Nil
  | x::xs -> List.fold_left (fun a b -> insert_by f b a) Nil lst

(* This function combines all values from a tree into a single value. *)
let rec reduce (tree: 'a btree) (elem: 'b) (f: 'b -> 'a -> 'b -> 'b) : 'b =
  match tree with
  | Nil -> elem
  | Leaf a -> f (reduce Nil elem f) a (reduce Nil elem f)
  | Fork (tree, a, tree2) -> f (reduce tree elem f) a (reduce tree2 elem f)

(* This function converts the tree values into a list of values. *)
let to_list (tree: 'a btree) = 
  reduce tree [] (fun tree elem tree2 -> tree @ [elem] @ tree2)


let () = 
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = 
              insert_by compare 4 Nil);
    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                    ) 
            );
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));


    (* Add more asserts here as you need them *)

    assert (insert_by compare 2 (insert_by compare 4 (insert_by compare 3 Nil)) =
              Fork (Leaf 2, 3, Leaf 4));
    assert (from_list compare [] = Nil);
    assert (from_list compare [5;5;5;5;5;5] = Leaf 5);
    assert (from_list compare [5;6;10;12;3;5;192;2;2;6;44] =
              Fork (Fork (Leaf 2, 3, Nil), 5,
              Fork (Nil, 6, Fork (Nil, 10, Fork (Nil, 12, Fork (Leaf 44, 192, Nil)))))
            );
    assert (List.sort compare [1;2;3;4;5;10;9;8;7;6] =
              to_list (from_list compare [10;8;7;6;4;2;1;3;5;7;9]));
    assert (List.sort compare [5] =
              to_list (from_list compare [5;5;5;5;5;5;5]));


    (* testing with letters *)
    assert (insert_by compare 'a' Nil = Leaf 'a');
    assert (insert_by compare 'b' (insert_by compare 'a' Nil) =
              Fork (Nil, 'a', Leaf 'b'));
    assert (insert_by compare 'a' (insert_by compare 'd' Nil) =
              Fork (Leaf 'a', 'd', Nil));       
    assert (insert_by compare 'b' (insert_by compare 'c' (insert_by compare 'a' Nil)) =
              Fork (Nil, 'a', Fork (Leaf 'b', 'c', Nil)));
    assert (from_list compare [] = Nil);
    assert (from_list compare ['d';'z'; 'z'] = Fork (Nil, 'd', Leaf 'z'));
    assert (from_list compare ['a';'b';'c';'e';'d';'f';'p';'c';'g'] = 
              Fork (Nil, 'a', Fork (Nil, 'b', Fork (Nil, 'c',
              Fork (Leaf 'd', 'e', Fork (Nil, 'f', Fork (Leaf 'g', 'p', Nil)))))));
    assert (List.sort compare ['z';'x';'d';'e';'u';'h'] =
              to_list (from_list compare ['z';'x';'d';'e';'u';'h']));
    assert (List.sort compare ['e'] =
              to_list (from_list compare ['e';'e';'e']));

    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg


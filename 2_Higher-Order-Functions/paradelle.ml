(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
  'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)

(* length function takes a list and returns its length as a value of type int *)
let length (lst: 'a list) : int =
  List.fold_left (fun a b -> a + 1) 0 lst

(* rev function takes a list and returns its list as reversed  *)
let rev (lst: 'a list) : 'a list =
  List.fold_left (fun a b -> b::a) [] lst

(* The first argument is a function to check if an element in the list (the third argument)
matches the values of the second argument. It will return true if any element in the list
"matches" (based on what the first argument determines) an element in the list. *)
let is_elem_by (f: 'a -> 'b -> bool) (searching_elem: 'a) (lst: 'a list) : bool =
  List.fold_left (fun a b -> a || f b searching_elem) false lst
  
(* is_elem whose first argument is a value and second argument is a list of values of the
same type. The function returns true if the value is in the list. *)
let is_elem (searching_elem: 'a) (lst: 'a list) : bool = 
  is_elem_by (=) searching_elem lst

(* dedup function takes a list and removes all deduplicates from the list *)
let dedup (lst: 'a list) =
  List.fold_right (fun a b -> if is_elem a b then b else [a] @ b) lst ([])

(* split_by function takes equality checking function, a list of values that are to be separated,
and and a list of separators values as inputs. *)
let split_by (f: 'a -> 'b -> bool) (lst: 'a list) (separators: 'b list)  =
  let loop elem new_lst =
    match new_lst with
    | [] -> []
    | x::xs ->  if is_elem_by f elem separators then []::x::xs else (elem::x)::xs
  in
  List.fold_right loop lst [[]]

(* takes out n elements in the list *)
let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

(* drops out 0-(n-1) elements in the list *)
let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l


(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

type result = OK 
	    | FileNotFound of string
	    | IncorrectNumLines of int 
	    | IncorrectLines of (int * int) list
	    | IncorrectLastStanza

type word = char list
type line = word list

(* removes all unwanted punctuations and new lines *)
let convert_to_non_blank_lines_of_words (lst: word) : line list =
  let lowercase lst0 = List.map (fun x -> Char.lowercase_ascii x) lst0
  in
  let split_by_punctuation lst1 = split_by (=) lst1 [' '; '.'; '!'; ','; ';'; ':'; '-']
  in
  let split_by_new_line lst2 =  split_by (=) lst2 ['\n'; '\r']
  in
  (* removes empty word*)
  let f_matching = fun x ->
    match x with
      | [] -> false
      | []::[] -> false
      | _::_ -> true
  in
  (* removes empty letters *)
  List.map (fun x -> 
    match List.filter (fun y -> y != []) x with
    | [] -> []
    | z -> z
  ) (List.filter f_matching (List.map split_by_punctuation (split_by_new_line (lowercase lst))))

  
(* gets all the words from a specified line *)
let get_content_from_line (n: int) (text_contents: 'a list) : 'a list =
  drop (n-1) (take n text_contents) 

(* checks if 2 lists are totally equal to each other *)
let compare_two_lists (lst1: 'a list) (lst2: 'a list) : bool =
  (List.fold_left (fun a b -> a && is_elem b lst2) true lst1) && (List.fold_left (fun a b -> a && is_elem b lst1) true lst2)

(* counts how many elements in a list *)
let how_many_duplicates_in_list (elem: 'a) (lst: 'a list) : int =
  List.fold_left (fun a b -> if b = elem then a + 1 else a) 0 lst

(* checks the unique words from lines 5 & 6 against unique words from lines 1-4 *)
let is_last_2_lines_different_as_first_4 (line_number: int) (text_contents: 'a list) : bool  =
  (* concatenates lines 1 & 3 *)
  let lines_1_and_3 = 
    List.fold_left (fun a b -> a @ List.concat (get_content_from_line b text_contents)) [] [line_number; line_number + 2]
  in
  let lines_5_to_6 = 
    (* concatenates lines 5 & 6 *)
    let words = 
      List.fold_left (fun a b -> a @ List.concat (get_content_from_line b text_contents)) [] [line_number + 4; line_number + 5]
    in
    (* checks if lines 5 & 6 words are within lines 1 & 3  *)
    let words_confirmed = 
      List.filter (fun x -> how_many_duplicates_in_list x words <= how_many_duplicates_in_list x lines_1_and_3) words
    in
    if length words = length words_confirmed
    then words
    else []
  in
  not 
  (compare_two_lists (dedup lines_1_and_3) (dedup lines_5_to_6))
  
(* checks the unique words from lines 1-18 against unique words from lines 19-24 *)
let is_last_stanza_good (text_contents: 'a list list) : bool  =
  (* concatenates lines the last 2 lines of each stanza and removes duplicates *)
  let first_three_stanzas = 
    dedup (List.fold_left (fun a b -> a @ List.concat (get_content_from_line b text_contents)) [] [5; 6; 11; 12; 17; 18])
  in
  (* concatenates lines 19-24 and removes duplicates *)
  let last_stanza = 
    dedup (List.fold_left (fun a b -> a @ List.concat (get_content_from_line b text_contents)) [] [19; 20; 21; 22; 23; 24])
  in
  (* compares between last stanza  *)
  compare_two_lists first_three_stanzas last_stanza

(* checks if the poem is a paradelle *)
let paradelle (filename: string) : result =
  match read_file filename with
  | None -> FileNotFound filename
  | Some content ->
    let text_contents = convert_to_non_blank_lines_of_words content
    in
    if length text_contents != 24
    then IncorrectNumLines (length text_contents)
    else
      let get_incorrect_lines =
        (* checks if line is empty or not & compares between current and next line, 1st-2nd & 3rd-4th line of every stanza *)
        let get_unmatched_lines = fun x -> 
          let current_line = get_content_from_line x text_contents
          in
          not (length current_line != 0 && current_line = get_content_from_line (x + 1) text_contents)
        in
        let unmatched = List.filter (get_unmatched_lines) [1; 3; 7; 9; 13; 15]
        in
        (* checks if the 5th-6th lines' unique words are the same as 1st-4th lines *)
        let last_two_lines_unmatched = List.filter (fun x -> is_last_2_lines_different_as_first_4 x text_contents) [1; 7; 13]
        in
        List.map (fun x ->
          if is_elem x unmatched 
          then (x, x + 1)       (* unmatched in either 1st/2nd or 3rd/4th line in the stanza *)
          else (x + 4, x + 5)   (* unmatched in the last 2 lines in the stanza *)
        ) (dedup (unmatched @ last_two_lines_unmatched))
      in 
      if length get_incorrect_lines != 0
      then IncorrectLines get_incorrect_lines
      else 
        if is_last_stanza_good text_contents
        then OK
        else IncorrectLastStanza
        
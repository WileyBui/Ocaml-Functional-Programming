#require "str"

(* Author: Eric Van Wyk (read_file, explode, and implode functions)
   Author: Wiley Bui    (other functions) 
  
   The word files, words-google-10000.txt and words-corncob.txt, 
   are taken from https://github.com/first20hours/google-10000-english 
   and http://www.mieliestronk.com/wordlist.html, respectively.
*)
let read_file (filename:string) : string list =
  let rec read_lines channel sofar =
    try
      let ln = input_line channel
      in read_lines channel (ln :: sofar)
    with
    | End_of_file -> sofar
    | e -> raise e
  in
  try 
    let channel = open_in filename
    in 
    let lines_in_reverse = read_lines channel []
    in List.rev lines_in_reverse
  with
  | e -> raise e

let rec explode : string -> char list = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let rec implode : char list -> string = function
  | []    -> ""
  | c::cs -> String.make 1 c ^ implode cs

let d1 = "words-small.txt"
let d2 = "words-google-10000.txt"
let d3 = "words-corncob.txt"

(* removes \r \n or \t from Windows machine  *)
let clean_file (filename: string) : string list =
  let strip_string s =
    Str.global_replace (Str.regexp "[\r\n\t ]") "" s
  in
  let rec clean_data (str_lst: string list) : string list =
    match str_lst with
    | [] -> []
    | x::xs -> strip_string x :: clean_data xs
  in clean_data (read_file filename)

type tuple_lst = (string * string) list  

(* Collects the word that has 4 char that also matches the 4 middle char
    from a 6 letter words: "rice", "prices". 
  *)
let rec collect_6_words (original_word_lst: string list) (words: string list) =
  let get_tuple (word: string) : tuple_lst =
    (match explode word with
      | c1::c2::c3::c4::c5::c6::[] when List.mem (implode [c2; c3; c4; c5]) original_word_lst -> 
          [(implode [c2; c3; c4; c5], word)]
      | _ -> []
    )
  in
  match words with
  | [] -> []
  | x::xs -> 
    if (String.length x == 6)
    then get_tuple x @ collect_6_words original_word_lst xs
    else collect_6_words original_word_lst xs

    
(* Returns [(4 CHAR WORD, 1 random char + 4 SAME CHAR WORD + 1 random char); ...] 
   by calling collect_6_words *)
let it_takes_two (filename: string) : tuple_lst =
  let data = clean_file filename
  in
  collect_6_words data data


(* This function measures the running time of another function.
   Found from: https://stackoverflow.com/a/9061574/ *)
let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx
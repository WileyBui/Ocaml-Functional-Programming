(* Author: Eric Van Wyk (read_file, explode, and implode functions)
   Author: Wiley Bui    (other functions) 
  
   The word files, words-google-10000.txt and words-corncob.txt, 
   are taken from https://github.com/first20hours/google-10000-english 
   and http://www.mieliestronk.com/wordlist.html, respectively. *)

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


type tuple_lst = (string * string) list

(*  This function takes in a list of words and computes to a list of tuples
    with the original and its sorted words: ["one"; "two"; "three"] returns
    [("one", "eno"); ("two", "otw"); ("three", "eehrt")] *)
let rec word_and_sorted_word_tuple (words: string list) : tuple_lst =
  match words with
  | [] -> []
  | x::xs -> (x, (implode (List.sort compare (explode x)))) :: word_and_sorted_word_tuple xs


(*  This function takes in a word list and [(word, sorted word)] tuple list.
    It iterates through the word list and also the tuple list to check if
    the word + "qu" is in the tuple list. If so, we append [(word, word with qu)]
    as a result.
*)
let rec match_words (data_lst: string list)
  (data_and_sorted_tuple: tuple_lst) : tuple_lst =
    let rec match_by_word (original_word: string)
      (look_up_sorted_word: string) (tuple: tuple_lst) : tuple_lst = 
        (match tuple with
        | [] -> []
        | (word, sorted_word)::ys -> 
            if look_up_sorted_word = sorted_word
            then [(original_word, word)]
            else match_by_word original_word look_up_sorted_word ys
        )
    in
    match data_lst with
    | [] -> []
    | x::xs ->
        let sorted_x = implode (List.sort compare (explode (x ^ "qu")))
        in
        (match_by_word x sorted_x data_and_sorted_tuple) @ 
            match_words xs data_and_sorted_tuple


(*  Returns [(word, word also containing "qu"); ...] by calling match_words *)
let qu_quiz (filename: string) =
  let data = read_file filename
  in
  match_words data (word_and_sorted_word_tuple data)

  
(*  This function measures the running time of another function.
    Taken from: https://stackoverflow.com/a/9061574/ *)
let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

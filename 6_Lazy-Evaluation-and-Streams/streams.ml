(* Constructing lazy values in OCaml *)

(* Lazy datatypes and functions *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = 
  ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


(* Some examples streams from files developed in class. *)
let rec from n =
  Cons ( n, delay ( fun () -> from (n+1) ) )

let ones =
  let rec mk_ones () = Cons (1, delay ( mk_ones ) )
  in mk_ones ()

let nats = from 1


(* Some helpful functions from files developed in class. *)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s :'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t) 
         )

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))


(* Below is a stream of factorials.  It uses, the same definition of
   factorials as we developed in class except that the built in
   multiplication operator is replaced by a function `mul_p` that
   still multiplies its arguments but prints out those arguments as
   well.  *)

let mul_p x y =
  let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
  in x * y

let rec factorials () =
  Cons (1, delay (fun () -> zip mul_p nats (factorials ())))

let facts = factorials ()

let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])



(*  cubes_from function takes an integer, n, and cubes that n.
    It then gets concatenated along from the stream. *)
let rec cubes_from (n: int) : int stream =
  Cons (n * n * n, delay (fun () -> cubes_from (n + 1)))

(*  cubes_from_map function uses the map function above, which
    takes the values through a function that multiples itself 3
    times, then populates another list with those results. *)
let cubes_from_map (n: int) : int stream =
  map (fun x -> x * x * x) (from n)

(*  cubes_from_zip function multiplies the values themselves by the zip
    function, and then multiplies themselves again by another zip function. *)
let cubes_from_zip (n: int) : int stream =
  zip ( * ) (zip ( * ) (from n) (from n)) (from n)

let () =
  assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
  assert (take 3 (cubes_from 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_zip 3) = [27; 64; 125])


let facts' = 
  let dummy = ref nats (* references of nats stream *)
  in
  (*  facts is "dereferencing" the nats values in dummy,
      and then zips its values to a list *)
  let facts = Cons (1, delay (fun () -> zip mul_p nats (! dummy)))
  in
  (*  dummy converts to stream reference *)
  let () = dummy := facts
  in
  facts
(*  `facts'` does not repeat the multiplications that `facts` does
    because `facts'` saves the previous values into memory. If the
    future values require the previous values, they can look it up
    from the memory rather than compute the previous again and again. *)

let () =
  assert (take 5 facts' = [1; 1; 2; 6; 24])


(*  The sift function returns a list of non-multiples of the
    integer, n, from the stream. It uses the filter function to
    filter out the non-multiples. *)
let sift (n: int) (stream: int stream) : int stream =
  let not_multiples (a: int) (b: int) : bool =
    not (b mod a = 0)
  in
  filter (not_multiples n) stream

(*  The sieve function returns a list of prime numbers
    by calling the sift function, starting the list with
    the head of the stream, then continuing with its
    recursive sieve calls from the tail values. *)
let rec sieve (stream: int stream) : int stream = 
  Cons ((head stream), delay (fun () -> sieve (sift (head stream) (tail stream))))

let primes = sieve (from 2)

let () =
  assert ( take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] )
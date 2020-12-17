open StreamModules

module type Hwk_06_Sig = sig
  type 'a stream

  val take: int -> 'a stream -> 'a list

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val facts: int stream
  val facts': int stream
  val primes: int stream
end

module Hwk_06(S: StreamSig) : Hwk_06_Sig = struct
  type 'a stream = 'a S.t
  let take = S.take
  let demand = S.demand

  let head = S.head
  let tail = S.tail
  let take = S.take
  let filter = S.filter
  let map = S.map
  let zip = S.zip

  let rec from n =
    S.Cons ( n, S.delay ( fun () -> from (n+1) ) )

  let ones = 
    let rec mk_ones () = S.Cons (1, S.delay ( mk_ones ) )
    in mk_ones ()
    
  let nats = from 1
  
  let mul_p x y =
    let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                              string_of_int y ^ ".")
    in x * y
  
  let rec factorials () =
    S.Cons (1, S.delay (fun () -> zip mul_p nats (factorials ())))
  
  let facts = factorials ()
  
  let rec cubes_from (n: int) : int stream =
    S.Cons (n * n * n, S.delay (fun () -> cubes_from (n + 1)))
  
  let cubes_from_map (n: int) : int stream =
    map (fun x -> x * x * x) (from n)
  
  let cubes_from_zip (n: int) : int stream =
    zip ( * ) (zip ( * ) (from n) (from n)) (from n)
  
  let facts' = 
    let dummy = ref nats (* references of nats stream *)
    in
    let facts = S.Cons (1, S.delay (fun () -> zip mul_p nats (! dummy)))
    in
    let () = dummy := facts
    in
    facts
  
  let sift (n: int) (stream: int stream) : int stream =
    let not_multiples (a: int) (b: int) : bool =
      not (b mod a = 0)
    in
    filter (not_multiples n) stream

  let rec sieve (stream: int stream) : int stream = 
    S.Cons ((head stream), S.delay (fun () -> sieve (sift (head stream) (tail stream))))
  
  let primes = sieve (from 2)
end
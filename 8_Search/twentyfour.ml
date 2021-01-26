type rat = int * int

type op = Add | Sub | Mul | Div

type expr
  = Rat of rat
  | BinOp of expr * op * expr 

let rat_add (n1,d1) (n2,d2) = (n1 * d2 + n2 * d1, d1 * d2)
let rat_sub (n1,d1) (n2,d2) = (n1 * d2 - n2 * d1, d1 * d2)
let rat_mul (n1,d1) (n2,d2) = (n1 * n2, d1 * d2)
let rat_div (n1,d1) (n2,d2) = (n1 * d2, d1 * n2)  

let gcd a' b' =
  let a = if a' < 0 then -a' else a' in
  let b = if b' < 0 then -b' else b' in
  let rec gcd' a b =
    if a = 1 || b = 1 then 1
    else
    if a = b
    then a
    else if a < b
    then gcd' a (b-a)
    else gcd' (a-b) b
  in gcd' a b
   
let rat_simplify (n,d) = 
  if n = 0 then (0,1) else
  let gcd_of_n_d = gcd n d 
  in  (n / gcd_of_n_d, d / gcd_of_n_d)


type evalError = DivByZero | FacOfNegativeNum | FacOfNonWholeNum
exception EvalError of evalError

let show_evalError : evalError -> string = function
  | DivByZero -> "Division by zero"
  | FacOfNegativeNum -> "Factorial of negative number"
  | FacOfNonWholeNum -> "Factorial of non-whole number"

let rec show (e: expr) : string =
  let show_op = function
    | Add -> " + "
    | Sub -> " - "
    | Mul -> " * "
    | Div -> " / "
  in
  match e with
    | BinOp (e1, op, e2) -> "(" ^ show e1 ^ show_op op ^ show e2 ^ ")"
    | Rat (n,d) -> 
       if d = 1 then string_of_int n
       else "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"

(* when an expression is found *)
exception FoundExpr of expr
exception KeepLooking

let rec eval (e: expr) =
  let divByZeroChecker n d =
    if d = 0
    then raise (EvalError DivByZero)
    else rat_simplify (n, d) 
  in
  match e with
  | Rat (n, d) -> divByZeroChecker n d
  | BinOp (ex1, op, ex2) ->
      (match op with
       | Add -> rat_add (eval ex1) (eval ex2)
       | Sub -> rat_sub (eval ex1) (eval ex2) 
       | Mul -> rat_mul (eval ex1) (eval ex2)
       | Div -> let evaluated = eval ex2
                in
                (match evaluated with
                | (n, d) -> divByZeroChecker d n; rat_div (eval ex1) evaluated
                )
      )

let e = BinOp (Rat (8, 1), Div, 
          BinOp (Rat (3, 1), Sub,
            BinOp (Rat (8, 1), Div, Rat (3, 1))))

let d = BinOp (Rat (8, 1), Div, 
          BinOp (Rat (3, 1), Sub,
            BinOp (Rat (7, 1), Sub, Rat (4, 1))))
let rec build_expr (ce:expr) (rest: rat list) : unit =
  (* print_endline ("Trying " ^ show ce) ; *)
  let ce_val : rat option = 
    try Some (rat_simplify (eval ce)) with
    | EvalError er -> print_endline (show_evalError er); None
  in
  match ce_val with
  | None -> ()
  | Some v ->
    if v = (24, 1) && rest = []
    then
      raise (FoundExpr ce)
    else
      (match rest with
      | [] -> ()
      | x::xs -> 
            let rec try_everything operators = 
              (match operators with 
              | [] -> ()
              | y::ys -> (match build_expr (BinOp (Rat x, y, ce)) xs with
                          | _ -> try_everything ys
                          )
              )
            in try_everything [Add; Sub; Div; Mul]
      )

let find_expr (lst: rat list) : expr option =
  match List.rev lst with
  | [] -> None
  | x::xs -> (try build_expr (Rat x) xs; None with
            | FoundExpr sln -> Some sln
            )
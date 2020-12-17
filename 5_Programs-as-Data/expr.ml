(* Hwk 05.  Extend the constructs below as specified. *)

 type value 
 = Int of int
 | Bool of bool
 | Closure of string * expr * value_environment
 | Ref of value ref

and value_environment = (string * value) list
                              
and expr 
 = Val of value

 | Add of expr * expr
 | Sub of expr * expr
 | Mul of expr * expr
 | Div of expr * expr 

 | Lt  of expr * expr
 | Eq  of expr * expr
 | And of expr * expr
 | Not of expr

 | Let of string * typ * expr * expr
 | Id  of string

 | App of expr * expr
 | Lam of string * typ * expr

 | LetRec of string * typ * expr * expr
 | If of expr * expr * expr

and typ = Int_type 
       | Bool_type
       | Func_type of typ * typ

type type_environment = (string * typ option) list 

type result = OK of typ
            | Errs of (expr * string) list

(* 
  Part 1.
  This unparse function converts an `expr` value into a string that is what we write in OCaml for similar computations.

  For example, `unparse (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))`
    should evaluate to `"(1 + (2 * 3))"`
*)
let rec unparse (e: expr) : string =
  let rec convert_types_to_string my_type = 
    match my_type with
        | Int_type -> "int"
        | Bool_type -> "bool"
        | Func_type (typ1, typ2) -> "(" ^ convert_types_to_string typ1 ^ " -> " ^ convert_types_to_string typ2 ^ ")"
  in
  match e with
  | Val (Int i)  -> string_of_int i
  | Val (Bool b) -> string_of_bool b
  | Val (Closure (str, e1, env)) -> "<fun>"
  | Val (Ref e) -> "reference"
  | Add (e1, e2) -> "(" ^ unparse e1 ^ " + " ^ unparse e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ unparse e1 ^ " - " ^ unparse e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ unparse e1 ^ " * " ^ unparse e2 ^ ")"
  | Div (e1, e2) -> "(" ^ unparse e1 ^ " / " ^ unparse e2 ^ ")"
  | Lt  (e1, e2) -> "(" ^ unparse e1 ^ " < " ^ unparse e2 ^ ")"
  | Eq  (e1, e2) -> "(" ^ unparse e1 ^ " = " ^ unparse e2 ^ ")"
  | And (e1, e2) -> "(" ^ unparse e1 ^ " && " ^ unparse e2 ^ ")"
  | Not (e1)     -> "(not " ^ unparse e1 ^ ")"
  | Let (str, my_type, e1, e2) ->
      "(let " ^ str ^ " : " ^ convert_types_to_string my_type ^  " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
  | Id  (str)    -> str
  | App (e1, e2) -> "(" ^ unparse e1 ^ " "   ^ unparse e2 ^ ")"
  | Lam (str, my_type, e1) ->  
      "(fun (" ^ str ^ ": " ^ convert_types_to_string my_type ^ ") -> " ^ unparse e1 ^ ")"
  | LetRec (str, my_type, e1, e2) -> "(let rec " ^ str ^ " : " ^ convert_types_to_string my_type ^  " = " ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
  | If (e1, e2, e3) -> "(if " ^ unparse e1 ^ " then " ^ unparse e2 ^ " else " ^ unparse e3 ^ ")"
  

(* 
  Part 2.
  freevars takes only one input, an expr, and returns a string list containing the free variables in that expression.
*)       
let rec freevars (e: expr) : string list =
  match e with
  | Val _ | If (_, _, _)-> []
  | Id x -> [x]
  | Not (e1) -> freevars e1
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) | Lt (e1, e2) | Eq (e1, e2) | And (e1, e2) | App (e1, e2)
      -> freevars e1 @ freevars e2
  | Let (str, my_type, e1, e2) | LetRec (str, my_type, e1, e2) -> freevars e1 @ List.filter (fun x -> x <> str) (freevars e2)
  | Lam (str, my_type, e1) -> List.filter (fun x -> x <> str) (freevars e1)

      
(* Part 3. Type checking *)           

let expect_Int (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> []
  | OK Bool_type | OK (Func_type (_, _)) ->  [(e, "expected Int type") ]
  | Errs errs -> errs

let expect_Bool (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type | OK (Func_type (_, _)) -> [(e, "expected Bool type") ]
  | OK Bool_type ->  []
  | Errs errs -> errs
  
let rec lookup (x: string) (env: type_environment) : result =
  match env with
  | [] | (_, None)::_ -> Errs ( [(Id x, "identifier not found")] )
  | (y, Some ty)::ys -> if x = y then OK ty else lookup x ys

let rec type_check (e:expr) (env: type_environment) : result =
  match e with
  | Id str -> lookup str env
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Val (Ref _) | Val (Closure (_, _, _)) | Not _ -> Errs [(e, "Ref, Closure, and Not pattern matching yet implemented")]
  | Eq (e1, e2) ->
    ( match type_check e1 env, type_check e2 env with
      | OK Int_type, (OK Int_type | OK Bool_type) -> OK Bool_type
      | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
    )
  | And (e1, e2) ->
    ( match type_check e1 env, type_check e2 env with
      | OK Bool_type, OK Bool_type -> OK Bool_type
      | r1, r2 -> Errs (expect_Bool r1 e1 @ expect_Bool r2 e2)
    )
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
    ( match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Int_type
      | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
    )
  | Lt (e1, e2) -> 
    ( match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Bool_type
      | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
    )
  | Lam (n, t, e1) ->
    ( match t with
      | Int_type -> 
        ( match type_check e1 ((n, Some Int_type)::env) with
          | OK Int_type -> OK Int_type
          | OK Bool_type | OK (Func_type (_, _)) -> Errs [(e, "expected Int")]
          | Errs r -> Errs r
        )
      | Bool_type -> 
        ( match type_check e1 ((n, Some Bool_type)::env) with
          | OK Bool_type -> OK Bool_type
          | OK Int_type | OK (Func_type (_, _)) -> Errs [(e, "expected Bool")]
          | Errs r -> Errs r
        )
      | Func_type (_, _) -> Errs [(e, "Func (_, _) not yet implemented")]
      
    )
  | App (e1, e2) ->
    ( match e1, type_check e1 env, type_check e2 env with
      | Val _, _, _ -> Errs [(e, "first argument of App(arg1, arg2) must contain Id constructor.")]
      | _, OK Int_type, OK Int_type -> OK Int_type
      | _, OK Int_type, OK Bool_type -> Errs [(e, "type mismatch in function application")]
      | _, OK Int_type, r1 -> Errs (expect_Int r1 e1)
      | _, OK Bool_type, OK Bool_type  -> OK Bool_type
      | _, OK Bool_type, r1 -> Errs (expect_Bool r1 e1)
      | _, OK (Func_type (Int_type, Int_type)), OK Int_type -> OK Int_type
      | _ ->  Errs [(e, "Wildcard (see App)")]
    )
  | If (e1, e2, e3) -> 
    ( match type_check e1 env, type_check e2 env, type_check e3 env with
      | OK Bool_type, OK Bool_type, OK Bool_type -> OK Bool_type
      | OK Bool_type, OK Int_type, OK Int_type -> OK Int_type
      | OK Bool_type, OK Int_type, r1 -> Errs [(e, "type mismatch in then and else branches of if")]
      | r1, _, _ -> Errs (expect_Bool r1 e1)
    )
  | Let (n, t, e1, e2) | LetRec (n, t, e1, e2) -> 
    ( match t with
      | Int_type -> 
        ( match type_check e1 env, type_check e2 ((n, Some Int_type)::env) with
          | OK Int_type, OK Int_type  -> OK Int_type
          | OK Int_type, OK Bool_type -> OK Bool_type
          | Errs x, _ -> Errs x
          | _, Errs y -> Errs y
          | _ ->  Errs [(e, "Wildcard (see Let #1)")]
        )
      | Bool_type -> 
        ( match type_check e1 env, type_check e2 ((n, Some Bool_type)::env) with
          | OK Bool_type, OK Bool_type -> type_check e2 ((n, Some Bool_type)::env)
          | OK Bool_type, Errs r1 ->  Errs r1
          | _, _ -> Errs [(e, "type mismatch in let expression")]
        )
      | Func_type (t1, t2) -> 
        ( match t1, t2 with
          | Int_type, Int_type -> 
              ( match e with
                | Let (n, t, e1, e2) -> 
                  ( match type_check e1 env, type_check e2 ((n, Some Int_type)::env) with
                    | OK Int_type, OK Int_type -> OK Int_type
                    | OK Int_type, Errs x -> Errs x
                    | _ -> Errs [(e, "Wildcard (see Let #2)")]

                  )
                | LetRec (n, t, e1, e2) ->
                  ( match type_check e1 ((n, Some Int_type)::env), type_check e2 ((n, Some Int_type)::env) with 
                    | OK Int_type, OK Int_type -> OK (Func_type (Int_type, Int_type))
                    | Errs r2, _ -> Errs r2
                    | _ -> Errs [(e, "Wildcard (see LetRec)")]
                  )
              )
          | _, _ -> Errs [(e, "Func_type pattern matching yet implemented")]
        )
    )


let check e = type_check e [] 
(* let evaluate e = eval [] e *)

(* Part 4. Evaluation *)

(* let rec eval (env: value_environment) (e:expr) : value =
 match e with
 | Val v -> v

 | Add (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int v1, Int v2 -> Int (v1 + v2)
      | _ -> raise (Failure "incompatible types, Add")
    )

 | Let (n, t, dexpr, body) ->
     let v = eval env dexpr in
     eval ( (n,v)::env ) body

 | _ -> failwith "complete this function" *)




(* some sample expressions *)

let e1 = Add (Val (Int 3), Val (Int 5))
let e2 = Add (Val (Int 3), Val (Bool true))
let e3 = Mul (Val (Bool true), Val (Int 5))
let e4 = Add (e2, e3)

let e5 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
             Add (Id "x", Val (Int 5))
          )
      
let e6 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
             Lt (Id "x", Val (Int 5))
          )
      
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x", Bool_type,
             Lt (Val (Int 3), Val (Int 5)),
             And (Id "x",
                  Let ("x", Int_type,
                       Add (Val (Int 1), Val (Int 2)),
                       Eq (Id "x", Val (Int 3))
                      )
                 )
            )

(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e8 = Let ("x", Bool_type,
             Lt (Val (Int 3), Val (Int 5)),
             And (Id "y",
                  Let ("x", Int_type,
                       Add (Val (Int 1), Val (Int 2)),
                       Eq (Id "x", Val (Int 3))
                      )
                 )
            )

let e9 = Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))

let e10 = If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))

let e11 = Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))

let e12 = Let ("x", Int_type,Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))

let e13 = Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))

let e14 = Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))

let e15 = If (Val (Int 4), Val (Int 0), Val (Int 1))

let e16 = If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))

let e17 = LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")

let e18 = Eq (Val (Bool false), Val (Bool true))


let err_1 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
                And (Id "x", Val (Bool true))
             )

let err_2 = Let ("x", Int_type, Add (Id "x", Val (Int 4)),
                And (Id "y", Val (Bool true))
             )

let inc_use = Let ("inc", Func_type (Int_type, Int_type), 
                  Lam ("n", Int_type, Add (Id "n", Val (Int 1))),
                  App (Id "inc", Val (Int 3))
               )

let sumToN : expr =
   LetRec ("sumToN", Func_type (Int_type, Int_type),
           Lam ("n", Int_type,
                If (Eq (Id "n", Val (Int 0)),
                    Val (Int 0),
                    Add (Id "n",
                         App (Id "sumToN",
                              Sub (Id "n", Val (Int 1))
                             )
                        )
                   )
               ),
           Id "sumToN"
          )

let sumTo3 = App (sumToN, Val (Int 4))



let () = 
  print_string "+ Testing unparse function ... " ;
  try
    assert (unparse e1 = "(3 + 5)");
    assert (unparse e2 = "(3 + true)");
    assert (unparse e3 = "(true * 5)");
    assert (unparse e4 = "((3 + true) + (true * 5))");
    assert (unparse e5 = "(let x : int = (3 + 4) in (x + 5))");
    assert (unparse e6 = "(let x : int = (3 + 4) in (x < 5))");
    assert (unparse e7 = "(let x : bool = (3 < 5) in (x && (let x : int = (1 + 2) in (x = 3))))");
    assert (unparse e8 = "(let x : bool = (3 < 5) in (y && (let x : int = (1 + 2) in (x = 3))))");
    assert (unparse e9 = "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc 3))");
    assert (unparse e10 = "(if (4 = 0) then 0 else 1)");
    assert (unparse e11 = "(let x : int = (3 + 4) in (x && true))");
    assert (unparse e12 = "(let x : int = (x + 4) in (y && true))");
    assert (unparse e13 = "(let x : bool = (3 * 5) in (x + 3))");
    assert (unparse e14 = "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc true))");
    assert (unparse e15 = "(if 4 then 0 else 1)");
    assert (unparse e16 = "(if (4 = 4) then 0 else false)");
    assert (unparse e17 = "(let rec sumToN : (int -> int) = (fun (n: int) -> (if (n = 0) then 0 else (n + (sumToN (n - 1))))) in sumToN)");
    assert (unparse e18 = "(false = true)");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg

let () = 
  print_string "+ Testing freevars function ... " ;
  try
    assert (freevars e1 = []);
    assert (freevars e2 = []);
    assert (freevars e3 = []);
    assert (freevars e4 = []);
    assert (freevars e5 = []);
    assert (freevars e6 = []);
    assert (freevars e7 = []);
    assert (freevars e8 = ["y"]);
    assert (freevars e9 = []);
    assert (freevars e10 = []);
    assert (freevars e11 = []);
    assert (freevars e12 = ["x"; "y"]);
    assert (freevars e13 = []);
    assert (freevars e14 = []);
    assert (freevars e15 = []);
    assert (freevars e16 = []);
    assert (freevars e17 = []);
    assert (freevars e18 = []);
    assert (freevars inc_use = []);
    assert (freevars sumToN  = []);
    assert (freevars sumTo3  = []);
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg

let () = 
  print_string "+ Testing type_checking function ... " ;
  try
    assert (check e1 = OK Int_type);
    assert (check e2 = Errs [(Val (Bool true), "expected Int type")]);
    assert (check e3 = Errs [(Val (Bool true), "expected Int type")]);
    assert (check e4 = Errs [ (Val (Bool true), "expected Int type");
                              (Val (Bool true), "expected Int type")]);
    assert (check e5 = OK Int_type);
    assert (check e6 = OK Bool_type);
    assert (check e7 = OK Bool_type);
    assert (check e8 = Errs [(Id "y", "identifier not found")]);
    assert (check e9 = OK Int_type);
    assert (check e10 = OK Int_type);
    assert (check e11 = Errs [(Id "x", "expected Bool type")]);
    assert (check e12 = Errs [(Id "x", "identifier not found")]);
    assert (check e13 = Errs [(Let ("x", Bool_type, Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3))), "type mismatch in let expression")]);
    assert (check e14 = Errs [(App (Id "inc", Val (Bool true)), "type mismatch in function application")]);
    assert (check e15 = Errs [(Val (Int 4), "expected Bool type")]);
    assert (check e16 = Errs [(If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false)), "type mismatch in then and else branches of if")]);
    assert (check e17 = OK (Func_type (Int_type, Int_type)));
    assert (check e18 = Errs [(Val (Bool false), "expected Int type"); (Val (Bool true), "expected Int type")]);
    assert (check inc_use = OK Int_type);
    assert (check sumToN  = OK (Func_type (Int_type, Int_type)));
    assert (check sumTo3  = OK Int_type);
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
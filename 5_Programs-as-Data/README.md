# Programs as Data

## Writing transparent code

We have discussed the importance of writing code in a manner that
makes it easier for others to understand it.  We've even had a few
labs devoted to this topic in which you improved your own code.  For
this assignment, part of your score will be based on how well you
write code that others can follow.

You should consider writing type annotations where they are useful to
others, consider using let-expressions to give intermediate values
names that help reader understand their intended use, not write lines
longer than 80 characters, along with using other means that we've
discussed for making your code easier to read.

You do not need to over-think this process, but take some care to make
your programs readable to others.


## Part 1: Unparse expressions

This problem asks you to write a function named ``unparse`` that
converts an ``expr`` value into a string that is what we write
in OCaml for similar computations.

For example,
```
unparse (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))
```
should evaluate to
```
"(1 + (2 * 3))"
```
Notice that ``unparse`` generates more parentheses than are needed.
This simplifies the implementation of ``unparse``.

Your function should generate output that you can copy and paste into
the OCaml interpreter (in utop) to **compute the value** of the expression.

By copying the output ``(1 + (2 * 3))`` from above, OCaml will display
``7``.

Constructs such as ``Lam`` and ``Let`` that have ``string``
components will need to generate the actual name without double quotes
in the resulting string.

For example,
```
unparse (Lam ("n", Int_type, Add(Id "n", Val (Int 1))))
```
should evaluate to
```
"(fun (n: int) -> (n + 1))"
```
To test your function, you can copy and paste your output into utop
and let OCaml evaluate the expressions. For example, you can paste
the output ``(fun (n: int) -> (n + 1))`` in utop and you should get a function of type
``int -> int``.

For another example,
```
unparse (App (Lam ("n", Int_type, Add (Id "n", Val (Int 1))), Val (Int 4)))
```
should evaluate to
```
"((fun (n: int) -> (n + 1)) 4)"
```
If you paste ``((fun (n: int) -> (n + 1)) 4)`` into utop, you
should get the value ``5``.


Note: In writing ``unparse`` you may need a helper function to convert
types into strings as well.

Note: Values built by the ``Closure`` constructor should be unparsed to
the string ``<fun>`` just like OCaml does for functional values.

Note: Values buld by the ``Ref`` cosntructor should be unparsed to just the
string ``reference``.  As with ``Closure`` values, these only arise
during the evaluation of an expression and are not inteneded to found
in any ``expr`` that one would consider a program in our small language.


## Part 2: Another way to compute free variables
Write a function named ``freevars`` that takes only one input, an ``expr``, and returns a ``string list`` containing the free variables in that expression.

This function should differ from ``free_vars`` that we wrote in class.
That in-class function called a helper function that passed along a
second argument to the helper that was the list of names bound by a
``let`` expression.  Then when an ``Id`` was encountered, this list
could be referenced to decide if this identifier should returned in a
singleton list (because it is not in the list and was thus free) or if
the empty list should be returned (because it is in the list and was
thus not free).

The function you are to write **should not work in this way**.
Instead, an ``Id "x"`` construct should **always** return ``[ "x" ]``
when given as an input to ``freevars``.  The ``let`` expression
construct is then responsible to examine the list of free variables
returned for the body and remove any mention of the variable that is
now being bound by this ``let`` expression.


## Part 3: Type checking

Recall, you need to do **either** part 3 or part 4, but not both.

Write a function called ``type_check`` with the type
``expr -> type_environment -> result``

The ``expr.ml`` file contains a number of sample expressions and the
results of type checking them is given below:

```
type_check e1 [] = OK Int_type

type_check e2 [] = Errs [(Val (Bool true), "expected Int type")]

type_check e3 [] = Errs [(Val (Bool true), "expected Int type")]

type_check e4 [] =
   Errs
    [(Val (Bool true), "expected Int type");
     (Val (Bool true), "expected Int type")]

type_check e5 [] = OK Int_type

type_check e6 [] = OK Bool_type

type_check e7 [] = OK Bool_type

type_check e8 [] = Errs [(Id "y", "identifer not found")]

type_check inc_use [] = OK Int_type

type_check sumToN [] = OK (Func_type (Int_type, Int_type))
```

## Part 4: Evaluation

Recall, you need to to **either** part 3 or part 4, but not both.

This problem asks you to complete the ``eval`` function to evaluate
all forms of expression.

In ``expr.ml`` you will find the definitions of the ``expr``,
``value``, and ``value_environment`` types.  There is also an
``evaluate`` function that simply calls ``eval`` for an expression
starting with the empty environment.

Some parts of ``eval`` are provided, but many of the interesting cases
need to be implemented by you.


### Step 1 - arithmetic expressions

To get started, first ensure that ``evaluate`` will work for the
arithmetic operations and integer constants.  Much of the work for this
has been done in some of the in-class examples already.

An example evaluation:

+ ``evaluate (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Int 7``


### Step 2 - logical and relational expressions

Logical and relational operations are also straightforward:

Some sample evaluations:

+ ``evaluate (Eq (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Bool false``

+ ``evaluate (Lt (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Bool true``

### Step 3 - conditional expressions
Conditional expressions should also pose not significant challenge.  
For example 

```
evaluate 
 (If (Lt (Val (Int 1), Mul (Val (Int 2), Val (Int 3))),
     Val (Int 4),
     Val (Int 5)))
```
evaluates to ``Int 4  ``


### Step 4 - let expressions

We have implemented non-recursive let-expressions in a forms in
class. Adapting that work to this setting should be straightforward.


### Step 5 - non-recursive functions

We have spent some time in class discussing closures as the way to
represent the value of a lambda expression.  The slides have several
examples of this, a few of which are reproduced here.  There is
additional information in the lecture slides.

The values ``inc`` and ``add`` are defined as follows:
```
let inc = Lam ("n", Int_type, Add(Id "n", Val (Int 1)))

let add = Lam ("x", Int_type,
               Lam ("y", Int_type, Add (Id "x", Id "y"))
              )
```

Some sample evaluations:

+ ``evaluate inc``
  evaluates to ``Closure ("n", Add (Id "n", Val (Int 1)), [])``

+ ``evaluate add``
  evaluates to ``Closure ("x", Lam ("y", Int_type, Add (Id "x", Id "y")), [])``

+ ``evaluate (App (add, Val (Int 1)))``
  evaluates to ``Closure ("y", Add (Id "x", Id "y"), [("x", Int 1)])``

+ ``evaluate  (App ( (App (add, Val (Int 1))), Val (Int 2)))``
  evaluates to ``Int 3``


### Step 6 - recursive functions

Consider the ``sumToN`` function we discussed in class.  In OCaml,
we'd write this function as follows:
```
let rec sumTo = fun n ->
      if n = 0 then 0 else n + sumTo (n-1)
in sumTo
```
To represent this function in our mini-OCaml language defined by the
``expr`` type, we'd represent the function as follows:
```
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
```
Here we've given the name ``sumToN`` to the ``expr`` value that
represent our ``sumTo`` function.

Thus,
+ ``evaluate (App (sumToN, Val (Int 10)))``
  evaluates to ``Int 55``



#### Somethings to consider

First, you can assume that the binding expression in a ``LetRec`` is a
``Lam`` expression.  Thus you could use the following skeleton to
structure the processing of ``LetRec`` in the evaluation function.
```ocaml
  | LetRec (f, t, dexpr, body) ->
     (match dexpr with
      | Lambda (n, t, fbody) ->

        ... Here const the closure that will be the value of f.
            This will involve the use of references as seen in the
            slides.

        ... In this code block you have access to all the pieces you
            need for creating the closure.  There is need to call
            eval on dexpr since you know it is a Lambda.  You can just
            do that work here directly.

        ... Then use it to create the evnironment for evaluation
            body.

      | _ -> raise (Failure "let rec expressions must declare a function")
     )
```

Second, there are some places in ``eval`` where an expression may
evaluate to a ``Ref`` value.  For example, this happens for the names
of recursive functions.  You will need to add code (in this particular
places) to dereference the ``ref`` inside a ``Ref`` value and then use
that ``value`` in the evaluation process.  You might ask what happens
in evaluating an ``App`` when the first argument is the name of a
recursive function.  This value will be a ``Ref`` value and you need
to get access to the underlying ``Closure`` that is refers to.


Another question you may have is what to do for expressions that have
type erorrs in them.  For example, an expression like
```
App ( Val (Int 999), Val (Int 111) )
```
is one that will try to treat the integer ``999`` as a function and
apply it to ``111```.  This is a type error.

For expressions like these, ``raise`` a ``Failure`` exception with an
appropritate message of your choosing.  This can be done with either
of the following two OCaml expressions
```ocaml
raise (Failure "Error!")
```
or
```ocaml
failwith "Error!"
```
They are equivalent.


## Some sample outputs

Below are some sample OCaml expressions and how they are to be
unparsed, type checked, and evaluated.  Note that you do not need to
evaluate expressions that have type errors.

There are 3 kinds of type errors
1. identifier not found errors, which are self explanatory
2. unexpected type found errors, these should use a message that
   begins with "expected", followed by the type that was expected
3. type mismatch errors, in this case there is a mismatch between
type, neither one of which is to be considered an expected type

Note that you type errors need not be exactly like those below - they
only need to begin with "identifier", "expected", or "type mismatch"
for each of the 3 cases above.  They should be informative, however,
and be more than just one of the three required strings given here.

Unparsing can also vary from the examples given below - having extra
parenthesis in places that you do not see them below is OK.  The
requirement is that they be valid OCaml expressions that match the
input ``expr``.

```
OCaml expression # 1:
   Add (Val (Int 3), Val (Int 5))

unparse:
   "(3 + 5)"

type:
   int

value:
   8


-----------------

OCaml expression # 2:
   Add (Val (Int 3), Val (Bool true))

unparse:
   "(3 + true)"

type errors (1):
   (true, "expected Int type")


-----------------

OCaml expression # 3:
   Mul (Val (Bool true), Val (Int 5))

unparse:
   "(true * 5)"

type errors (1):
   (true, "expected Int type")


-----------------

OCaml expression # 4:
   Add (Add (Val (Int 3), Val (Bool true)), Mul (Val (Bool true), Val (Int 5)))

unparse:
   "((3 + true) + (true * 5))"

type errors (2):
   (true, "expected Int type")
   (true, "expected Int type")


-----------------

OCaml expression # 5:
   Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))

unparse:
   "(let x : int = (3 + 4) in (x + 5))"

type:
   int

value:
   12


-----------------

OCaml expression # 6:
   Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))

unparse:
   "(let x : int = (3 + 4) in (x < 5))"

type:
   bool

value:
   false


-----------------

OCaml expression # 7:
   Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))

unparse:
   "(let x : bool = (3 < 5) in  (x && (let x : int = (1 + 2) in (x = 3))))"

type:
   bool

value:
   true


-----------------

OCaml expression # 8:
   Let ("x", Bool_type,Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type,Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))

unparse:
   "(let x : bool = (3 < 5) in  (y && (let x : int = (1 + 2) in (x = 3))))"

type errors (1):
   (y, "identifer not found")


-----------------

OCaml expression # 9:
   Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))

unparse:
   "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc 3))"

type:
   int

value:
   4


-----------------

OCaml expression # 10:
   If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))

unparse:
   "(if (4 = 0) then 0 else 1)"

type:
   int

value:
   1


-----------------

OCaml expression # 11:
   Let ("x", Int_type,Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))

unparse:
   "(let x : int = (3 + 4) in  (x && true))"

type errors (1):
   (x, "expected Bool type")


-----------------

OCaml expression # 12:
   Let ("x", Int_type,Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))

unparse:
   "(let x : int = (x + 4) in  (y && true))"

type errors (1):
   (x, "identifer not found")


-----------------

OCaml expression # 13:
   Let ("x", Bool_type,Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))

unparse:
   "(let x : bool = (3 * 5) in (x + 3))"

type errors (1):
   ((let x : bool = (3 * 5) in (x + 3)), "type mismatch in let expression")


-----------------

OCaml expression # 14:
   Let ("inc", Func_type (Int_type, Int_type),Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))

unparse:
   "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc true))"

type errors (1):
   ((inc true), "type mismatch in function application")


-----------------

OCaml expression # 15:
   If (Val (Int 4), Val (Int 0), Val (Int 1))

unparse:
   "(if 4 then 0 else 1)"

type errors (1):
   (4, "expected Bool type")


-----------------

OCaml expression # 16:
   If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))

unparse:
   "(if (4 = 4) then 0 else false)"

type errors (1):
   ((if (4 = 4) then 0 else false), "type mismatch in then and else branches of if")


-----------------

OCaml expression # 17:
   LetRec ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")

unparse:
   "(let rec sumToN : (int -> int) = (fun (n: int) -> (if (n = 0) then 0 else (n + (sumToN (n - 1))))) in sumToN)"

type:
   (int -> int)

value:
   <fun>


-----------------

OCaml expression # 18:
   Eq (Val (Bool false), Val (Bool true))

unparse:
   "(false = true)"

type errors (2):
   (false, "expected Int type")
   (true, "expected Int type")
```




## An alternative view of the tests above

In the test cases shown above, the error messaages are printed by calling ``unparse`` on the ``expr`` inside of each error.

Thus, on test 18 above, we see ``false`` in the first error instead of ``Val (Bool false)``.  It is the second one - ``Val (Bool false)`` that is actually the first component of that first error.

I have re-run the code to generate the text above and replaced the call to ``unparse`` with one that serializes the expression so that it looks like what OCaml would print for that value.

This "convenience" of calling unparse was confusing to a few people and thus I'm providing a this second version of the 18 test cases above.

```
OCaml expression # 1:
   Add (Val (Int 3), Val (Int 5))

unparse:
   "(3 + 5)"

type:
   int

value:
   8


-----------------

OCaml expression # 2:
   Add (Val (Int 3), Val (Bool true))

unparse:
   "(3 + true)"

type errors (1):
   (Val (Bool true), "expected Int type")


-----------------

OCaml expression # 3:
   Mul (Val (Bool true), Val (Int 5))

unparse:
   "(true * 5)"

type errors (1):
   (Val (Bool true), "expected Int type")


-----------------

OCaml expression # 4:
   Add (Add (Val (Int 3), Val (Bool true)), Mul (Val (Bool true), Val (Int 5)))

unparse:
   "((3 + true) + (true * 5))"

type errors (2):
   (Val (Bool true), "expected Int type")
   (Val (Bool true), "expected Int type")


-----------------

OCaml expression # 5:
   Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Add (Id "x", Val (Int 5)))

unparse:
   "(let x : int = (3 + 4) in (x + 5))"

type:
   int

value:
   12


-----------------

OCaml expression # 6:
   Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), Lt (Id "x", Val (Int 5)))

unparse:
   "(let x : int = (3 + 4) in (x < 5))"

type:
   bool

value:
   false


-----------------

OCaml expression # 7:
   Let ("x", Bool_type, Lt (Val (Int 3), Val (Int 5)), And (Id "x", Let ("x", Int_type, Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))

unparse:
   "(let x : bool = (3 < 5) in  (x && (let x : int = (1 + 2) in (x = 3))))"

type:
   bool

value:
   true


-----------------

OCaml expression # 8:
   Let ("x", Bool_type, Lt (Val (Int 3), Val (Int 5)), And (Id "y", Let ("x", Int_type, Add (Val (Int 1), Val (Int 2)), Eq (Id "x", Val (Int 3)))))

unparse:
   "(let x : bool = (3 < 5) in  (y && (let x : int = (1 + 2) in (x = 3))))"

type errors (1):
   (Id "y", "identifer not found")


-----------------

OCaml expression # 9:
   Let ("inc", Func_type (Int_type, Int_type), Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Int 3)))

unparse:
   "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc 3))"

type:
   int

value:
   4


-----------------

OCaml expression # 10:
   If (Eq (Val (Int 4), Val (Int 0)), Val (Int 0), Val (Int 1))

unparse:
   "(if (4 = 0) then 0 else 1)"

type:
   int

value:
   1


-----------------

OCaml expression # 11:
   Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)), And (Id "x", Val (Bool true)))

unparse:
   "(let x : int = (3 + 4) in  (x && true))"

type errors (1):
   (Id "x", "expected Bool type")


-----------------

OCaml expression # 12:
   Let ("x", Int_type, Add (Id "x", Val (Int 4)), And (Id "y", Val (Bool true)))

unparse:
   "(let x : int = (x + 4) in  (y && true))"

type errors (1):
   (Id "x", "identifer not found")


-----------------

OCaml expression # 13:
   Let ("x", Bool_type, Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3)))

unparse:
   "(let x : bool = (3 * 5) in (x + 3))"

type errors (1):
   (Let ("x", Bool_type, Mul (Val (Int 3), Val (Int 5)), Add (Id "x", Val (Int 3))), "type mismatch in let expression")


-----------------

OCaml expression # 14:
   Let ("inc", Func_type (Int_type, Int_type), Lam ("n", Int_type, Add (Id "n", Val (Int 1))), App (Id "inc", Val (Bool true)))

unparse:
   "(let inc : (int -> int) = (fun (n: int) -> (n + 1)) in (inc true))"

type errors (1):
   (App (Id "inc", Val (Bool true)), "type mismatch in function application")


-----------------

OCaml expression # 15:
   If (Val (Int 4), Val (Int 0), Val (Int 1))

unparse:
   "(if 4 then 0 else 1)"

type errors (1):
   (Val (Int 4), "expected Bool type")


-----------------

OCaml expression # 16:
   If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false))

unparse:
   "(if (4 = 4) then 0 else false)"

type errors (1):
   (If (Eq (Val (Int 4), Val (Int 4)), Val (Int 0), Val (Bool false)), "type mismatch in then and else branches of if")


-----------------

OCaml expression # 17:
   LetRed ("sumToN", Func_type (Int_type, Int_type),Lam ("n", Int_type, If (Eq (Id "n", Val (Int 0)), Val (Int 0), Add (Id "n", App (Id "sumToN", Sub (Id "n", Val (Int 1)))))), Id "sumToN")

unparse:
   "(let rec sumToN : (int -> int) = (fun (n: int) -> (if (n = 0) then 0 else (n + (sumToN (n - 1))))) in sumToN)"

type:
   (int -> int)

value:
   <fun>


-----------------

OCaml expression # 18:
   Eq (Val (Bool false), Val (Bool true))

unparse:
   "(false = true)"

type errors (2):
   (Val (Bool false), "expected Int type")
   (Val (Bool true), "expected Int type")
```

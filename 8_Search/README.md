# Search

## Part 1. Collecting subset sum solutions: `subsetsum_exn_ref_all`

Write a function named `subsetsum_exn_ref_all` with the type `int list
-> int list list` that uses a call to our function
`subsetsum_exn_continuation` to collect all the solutions to the
subset sum problem for a particular input.

For example, the following `assert`s should be successful.
```
let () =
  assert (List.mem [1; 5; -6]
            (subsetsum_exn_ref_all [1; 3; -2; 5; -6]));
  assert (List.mem [3; -2; 5;-6] 
            (subsetsum_exn_ref_all [1; 3; -2; 5; -6]))
```

Recall the function `subsetsum_exn_all` in `search_exceptions.ml`.  It
passes a continuation to `subsetsum_exn_continuation` that prints out
each subset before raising an exception to keep searching for more
solutions.

Your function `subsetsum_exn_ref_all` should set up a reference that
the continuation function that your solution will pass to
`subsetsum_exn_continuation` will use to collect all of the solutions.
That is, instead of printing them out, the solution should be added to
the list in the reference.  Your `subsetsum_exn_ref_all` must return
this list of solutions.



## Part 2. Continuations in the wolf-goat-cabbage problem.

### Writing `crossing_continuation`

In the `search_wolf.ml` file we explored using options and exceptions
for solving the wolf-goat-cabbage problem, but we did not solve this
problem using continuations.  This problem asks you to do just that.

To get started, review the function `crossing_v1` that used options
for solving this problem.  Then make a copy of it, at the bottom of
your `search_wolf.ml` file and rename it `crossing_continuation`.

The work for this problem is to modify this function to use a success
continutation instead of options.

Start by changing the type of the function to read as follows:
```
let crossing_continuation (succ: state list -> 'a) : 'a = 
  ...
```

Now instead of taking a unit value `()` as input it takes a
continutation.  Similarly, instead of returning an option it returns
the produced by the continutaion function.

In completing the transformation of this function, note that
there should be no uses of option types anymore.  So looking to places where `Some` and `None` are used will give you some idea
of what parts of the body of the function that might need to change.

### Writing `crossing_all` to use `crossing_continuation`

Next, write a function named `crossing_all` that has the type `() ->
state list list`.  This function should use `crosing_continuation` to
collect all valid river crossings.  You may choose to model this
function on `subsetsum_exn_ref_all` that you wrote for part 1 of this
assignment.


## Part 3. Evaluating to 24

In this part, you are asked to solve a puzzle to find an expression in
which the four numbers are combined into an arithmetic expression that
evaluates to 24.

This work is to be done in a file named `twentyfour.ml`. You are free
to include the code snippets given below.

The four numbers are provided as input in a list, for example `[8; 3;
8; 3]`, and the solution is an expresion, such as `8 / (3 - (8 / 3))`
which evaluates to `24`.

The numbers must appear in the expression in the same order in which
they appear in the input list.

In this problem, the choices to make are how to take the next number
in the list and insert it, using some binary operator, into the
expression being constructed.

In the subset-sum problem, we kept track of a potential subset, called
`partial_subset` in our example programs, and made decisions about
adding or not adding the next number in the set into that partial
subset.

Here the decision is *how* we add each number into the expression that
is being constructed.

In the subset-sum problem we checked if the numbers in the partial
subset added up to 0, but here we are checking if the expression
evaluates to 24.


### Getting started.

There is a fair amount of material to digest to get started with this
problem.  I suggest you read the material below twice just to get a
good understanding of everything.

#### Rational numbers

Notice that in the example above we used division.  To avoid rounding
problems with either integer or floating-point division we will use
rational numbers in this problem.  Thus all the numeric values we will
use will be represented by the following `rat` type:

```ocaml
type rat = int * int
```

As indicated, a rational number is just a pair of integers,
representing, respectivly, the numerator and denominator, of a
fraction.  Thus the input value `8` is represented as `(8, 1)` and `3`
is represented as `(3, 1)`.  

Thus the expression that divides `(8, 1)` by `(3, 1)` will produce the
rational value `(8, 3)`.

The value of "one-half" is represented by `(1, 2)` or `(2, 4)` or
other representations. So not all rational numbers need to be in there
simplified form at all times during the computation.  But they should
always be simplified when displayed in output to a user of your
solution to this problem.


#### Expressions over rational numbers

Because this problem involves the generation and transformation of
expressions a slightly different, more generic, representation of
arithmetic expressions is useful here.  Below are the two types that
we will use for expressions.

```ocaml
type op = Add | Sub | Mul | Div

type expr
  = Rat of rat
  | BinOp of expr * op * expr 
```

Note that this representation of `expr` is different from those we saw
before in section 3 on "Programs as Data."  We have a generic `BinOp`
constructor that has an extra component to indicate what the operator
is.  This `op` type indicates if binary operation are addition,
substraction, multiplication, or division.


#### Expression evaluation

To know if an expression evaluates to the value 24 we need to,
obviously, evaluate expressions.  You thus need to write a function
named `eval` with the following type:
```ocaml
  expr -> rat
```

Since there are no variables we do not need any sort of environment
and this function is similar to the `eval` function in the
`arithmetic.ml` file in the `expr` directory in the sample programs
part of the public class repository.

There are some important differences however.

First, there are no built-in operation for adding, subtracting,
multiplying, or dividing rational numbers as we have defined them.
Thus, you will need to write you own versions of these.

In the evaluation of expression `e` you might have the following
clause in a `match` construct for `e`:

```ocaml
  BinOp (e1, Add, e2) -> rat_add (eval e1) (eval e2)   
```

You will have similar patterns where the 2nd argument of `BinOp` is
`Sub`, `Mul, and `Div`, each time calling an appropriate function for
arithmetic operations over rational numbers.

For example, a definition of `rat_add` might be as follows:
```ocaml
let rat_add (n1,d1) (n2,d2) = (n1 * d2 + n2 * d1, d1 * d2)
```

This function adds rational numbers in the regular way, but does not
simplify the result.  You might write a function for dividing rational
numbers that divides `(8, 1)` by `(2, 1)` to return `(8, 2)`.  But
this result could be simplified to `(4, 1)`.

Thus, we might like another function `rat_simplify` to simplify
rational numbers.  This, along with a `gcd` function is given below.
You should feel free to use this one, or improve upon it, in your
solution.

```ocaml
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
```

We might thus see these functions used in together.  To evaluate an
expression `e`, we might evaluate the following:
```ocaml
rat_simplify (eval e)
```

There is one last concern to address: evaluation that fails.  What do
we do when we divide a value by zero?  In OCaml, the evaluation of `8
/ 0` raise as `Division_by_zero` exception.  We will do something
similar, but with our own form of exceptions:

```ocaml
type evalError = DivByZero | FacOfNegativeNum | FacOfNonWholeNum

exception EvalError of evalError
```

If an attempt is made to perform a division of by zero (that is `(0,
anything)`) your `eval` function will raise an exception by evaluating
```ocaml
raise (EvalError DivByZero)
```

The two other constructors for `evalError` will be used in the
optional extra credit component that adds factorials into our
expression operators.  So just disregard `FacOfNegativeNum` and
`FacOfNonWholeNum` for now.


#### Displaying expressions and evaluation errors.

The following function may be useful in your work.  They are used in some of the example code provided further below in this document.

```ocaml
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
```


### OK, now the search problem!

The primary aim of this assignment is to write `find_expr` with type
```ocaml
rat list -> expr option
```
so that (NOTE, this call was originally incorrect, fixed 4:30pm on Dec 5)
```ocaml
find_expr [(8,1); (3,1); (8,1); (3,1)]
```
returns
```ocaml
Some
 (BinOp (Rat (8, 1), Div,
   BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))
```

This is our representation of the expression `8 / (3 - (8 / 3))`
mentioned above.

#### Some guidance on writing `find_expr`

Your solution should follow the examples we have developed in class to
use an exception to control the search.  Specifically, it should use
the following exception to be raised when a solution is found:
```ocaml
exception FoundExpr of expr
```

Your `find_expr` function is similar to many of the search functions
that we have written in class.  One similarity is that the real work
is done by a nested helper function.  In our first example of
`subsetsum_option_v1` in `search_options.ml` we had a nested helper
function named `try_subset`.  Here it is suggested to write a helper
function named `build_expr` that begins as follows:

```ocaml
  let rec build_expr (ce:expr) (rest: rat list) : unit =
    print_endline ("Trying " ^ show ce) ;
    let ce_val : rat option = 
      try Some (rat_simplify (eval ce)) with
      | EvalError er -> print_endline (show_evalError er); None
    in
    match ce_val with
    | None -> ()
    | Some v ->
       if v = (24 ,1) && rest = []
       then
         raise (FoundExpr ce)
       else
         ...
```

##### Some things to note about the code above:

- `ce` is the current expression that has been constructed so far.  It is a bit like thd `partial_subset` argument seen in previous examples.  The `ce` expression represents the current path in the search tree - the expression we have built up so far from the list of rational numbers given as input.

- `rest` is the set of remaining rational number to add into the expression ins some way.

- the return type of `build_expr` is `unit` since finding a solution will cause the `FoundExpr` exception to be raised.

- the `build_expr` function first print out the expression we have built up so far (using `show` defined above).

- it then tries to evaluate `ce`.  If this fails an error message is printed an `ce_val` get the value `None`.  If it succeeds the `rat` value is wrapped up in an option type of `Some`.

- If this evaluation failed, then this call to `build_expr` returns `()`, the unit value indicating that no solution was found.

- Otherwise we check if it evaluated to 24 and raise the `FoundExpr` exception if it did.

- If the value is not 24 then we must continue the search.  This is the part of the function that you need to complete.


##### A strategy for building up expressions.

Our aim is to start with a list of `rat` values
```
  [(8,1); (3,1); (8,1); (3,1)]
```
and construct and expression such as `(8 / (3 - (8 / 3)))` which is 
represented in our `expr` and `rat` types as:
```ocaml
 BinOp (Rat (8, 1), Div,
   BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1))))
```

Now look at the structure of this expression: `(8 / (3 - (8 / 3)))`.
Notice how the last number in the input list of `rat` values is the
most deeply nested number in the tree structure of the expression.

It seems that we started with the last number `(3,1)` then
1. constructed `8 / 3` - (that is `BinOp (Rat (8, 1), Div, Rat (3, 1))`
   by adding an 8 as the left child under a division and the first 
   expression constructs (the `3`) as the right child.
2. next we built up from `8 / 3` by making a subtraction expression with
   the next number (`3`) as the left child and the previous expression as the right child.
3. this process is repeated one more time by taking the next number (the `8` at the front of the list) and making it the left child under a division with the previously constructed expression as the right child.

So what we did in `find_expr` was
1. reverse the input list of rational numbers
2. take the head element of this reversed list (the initial `3`) and use
   it as the first expression passed into the first call to `build_expr` as the `ce` argument.
3. the tail of this reversed list is then the `rest` of the rational numbers to process.

The choices that we make at each step in `build_expr` is which of the
four operators (`Add`, `Sub`, `Mul`, or `Div`) to use when building up
the expression out of `ce` and the head element of `rest`.

##### Using `find_expr`

A correct implementation of `find_expr` will print out all the expressions that are considered in the search, even the ones that do not have all 4 required numbers in them, and return an `option` with a solution if one exists.

For `find_expr [(8,1); (3,1); (8,1); (3,1)]`, several expressions are printed before the function returns a value of type `expr option` with the value
```ocaml
Some
 (BinOp (Rat (8, 1), Div,
   BinOp (Rat (3, 1), Sub, BinOp (Rat (8, 1), Div, Rat (3, 1)))))
```

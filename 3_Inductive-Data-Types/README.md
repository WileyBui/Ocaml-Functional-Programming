# Inductive Data Types

## Part 1

### Getting Started

Create a file named ``trees.ml``.  Copy and paste the following declarations to it:
```
(* A tree type declaration. *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )
```

To do some simple testing of your code, add the following to the
bottom of this ``trees.ml`` file:
```
let () = 
  print_string "Testing part 1 ... " ;
  try
    (*
    assert (size str_tree = 4);
    assert (sum int_tree = 10);
    assert (product int_tree = 24);
    assert (charcount str_tree = 20);
    assert (concat str_tree = "I really love OCaml!");
    *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
```
This describes a value of type ``unit`` that is evaluated when you
load this file into ``utop``.  Try this now.  It says that your tests
passed, but only because all of the ``assert`` uses to do any testing
are commented out.

As you implement these functions, uncomment the appropriate ``assert``
call to run the test.


### ``size``
Write a function named ``size`` of type ``'a tree -> int`` that counts
the number of values stored in a tree.  These are just the values
stored in the ``Node``-constructed trees.

Uncomment the test
```
    assert (size str_tree = 4);
```
by moving the beginning-comment marker to the line below it.  Thus
running this test but not the others.  Now load it into ``utop`` and
see that it works.

We will put all tests like this at the bottom of the file so that the
results are the last thing printed out when the file is loaded into
``utop``.   If there declarations below these tests, then you may not
notice the result of testing when lots of declarations clutter up the
end of the what ``utop`` displays.

Add some additional ``assert`` calls to more completely test your
``size`` function.  Try to cover the different yet relevant kinds of
inputs your function may get.

### ``sum``
Write a function named ``sum`` of type ``int tree -> int`` that adds
together all the ``int`` values stored in an ``int tree``.  Uncomment
the relevant ``assert``, as you did for the ``size`` function above,
and load it into ``utop`` to check it.

Add some additional ``assert`` calls to more completely test your
``sum`` function.  Try to cover the different yet relevant kinds of
inputs your function may get.

### ``product``
Add the function named ``product`` of type ``int tree -> int`` as
follows to your ``trees.ml`` file:
```
let rec product (t: int tree) : int = 99
```
Now uncomment the ``assert`` the calls ``product`` and load the file
into ``utop``.  You can now see what happens when an error is detected
by these simple tests.  

By keeping all tests like this at the bottom of the file you can quite
quickly know if you functions are behaving as you intend.  You don't
need to type more expressions into ``utop`` to test your functions -
you just ``#use`` the file and see the test run.

Now fix the implementation of ``product`` so that it multiplies
together all the ``int`` values stored in an ``int tree``.  Check your
work by loading it into ``utop`` again.

Add some additional ``assert`` calls to more completely test your
``product`` function.  Try to cover the different yet relevant kinds
of inputs your function may get.

### ``charcount``
Write a function named ``charcount`` that determines the total number
of characters stored in the strings in a ``string tree``.  It should
have the type ``string tree -> int``.

Check this by uncommenting the appropriate ``assert``.

Add some additional ``assert`` calls to more completely test your
``charcount`` function.  Try to cover the different yet relevant kinds
of inputs your function may get.

### ``concat``
Write a function named ``concat`` that concatenates all the strings in
a ``string tree``.  It will have the type ``string tree -> string``.
Check it by uncommenting the appropriate ``assert``.

Add some additional ``assert`` calls to more completely test your
``concat`` function.  Try to cover the different yet relevant kinds of
inputs your function may get.



## Part 2. Trees with lists

In this part you'll write functions similar to those above but for
``tree``s in which the values are lists.


Add the following to your ``trees.ml`` file - these are some sample
trees with lists.
```
let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )
```

Add the following to the bottom of you ``trees.ml`` file, after the
part 1 tests.
```
let () = 
  print_string "Testing part 2 ... " ;
  try
    (*
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
     *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
```
Again, here are a collection of ``assert`` calls that will test the
functions described below.

For each function, uncomment the relevant ``assert`` and add
additional ones to cover other relevant test cases.

### ``list_tree_size``
This is a function with the type ``'a list tree -> int``
and counts the number of elements in the lists in the nodes in the
tree.

### ``list_tree_sum`` 
This is a function with the type ``int list tree -> int`` that adds up
all the ``int`` elements in the lists in the nodes of a ``int list
tree``. 

### ``list_tree_product`` 
This is a function with the type ``int list tree -> int`` that
multiplies all the ``int`` elements in the lists in the nodes of a
``int list tree``.

### ``list_tree_charcount``
This is a function with the type ``string list tree -> int`` that
counts the number of characters in the ``string`` lists inside a
``string list tree``.

### ``list_tree_concat``
This is a function with the type ``string list tree -> string`` that
concatenates all the strings in the ``string`` lists inside a ``string
list tree``.

For each of these 5 function you will want to add additional
``assert`` calls to further test your functions and cover a more
reasonable set of possible inputs.


## Part 3 - using ``reduce``

In this part you will write new implementations of the functions
above but now those functions cannot be recursive.  Instead they must
call a ``reduce`` function.   For the functions working on trees with
lists you will need to use functions such as ``fold_left`` or
``fold_right`` from the ``List`` or ``String`` module.  The only
restriction is that ``reduce`` can be the only ``let rec`` in your
``reduce.ml`` file.

### Getting started

Create a new file called ``reduce.ml``

Add the following declarations to the top of your new file:
```
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )

let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )
```

After these, add the following tests:
```
let () = 
  print_string "Testing part 3 ... " ;
  try
    (*
    assert (size str_tree = 4);
    assert (sum int_tree = 10);
    assert (product int_tree = 24);
    assert (charcount str_tree = 20);
    assert (concat str_tree = "I really love OCaml!");
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
    *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
```


### Writing new versions of the previous functions.

For each of the 10 functions from parts 1 and 2 you are to write a new
version that is **not** recursive but instead calls ``reduce``.

To recall, the functions are their types are as follows:
```
reduce : 'a tree -> 'b -> ('a -> 'b -> 'b -> 'b) -> 'b
size : 'a tree -> int
sum : int tree -> int
product : int tree -> int
charcount : string tree -> int
concat : string tree -> string
list_tree_size : 'a list tree -> int
list_tree_sum : int list tree -> int
list_tree_product : int list tree -> int
list_tree_charcount : string list tree -> int
list_tree_concat : string list tree -> string
```

For each one uncomment the ``assert`` as appropriate and copy the new
``assert`` calls from your solutions to parts 1 and 2 into the testing
code provided above.

Recall, keep the tests at the bottom of the file so that the last
thing that is displayed is the result of running your tests.



## Part 4 - sorted binary trees

### Getting Started

Create another file ``binary_trees.ml`` and add the following tree declaration to it.
```
type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree
```

We will use this data type to represent sets of values.  It is a
slightly unusual implementation of binary trees as it contains 3
constructs: ``Nil``, ``Leaf``, and ``Fork``.

There are three invariants that your code must maintain.

First, if a value exists in the tree, it only exists in one place.
That is, there are no duplicates in the tree. Thus the following tree
is OK:
```
Fork (Leaf 3, 2, Leaf 4)
```
but this tree violates this invariant:
```
Fork (Leaf 3, 2, Fork (Nil, 3, Leaf 4))
```

Second, no tree should have a tree inside of it that has the form
```
Fork (Nil, ...value..., Nil)
```
A tree like this must instead be represented using the ``Leaf``
constructor as follows:
```
Leaf (...value...)
```

Third, the elements in a tree must be sorted.  ``Nil`` and ``Leaf``
trees are sorted by definition.  A ``Fork`` tree is sorted if
1. all elements in the left tree are smaller than the value on the
   ``Fork`` node 
2. all elements in the right tree are larger than the value on the
   ``Fork`` node 
3. all nested tree are also sorted.

This leads to the obvious question: what do we mean by "smaller" and
"larger"?  When trees in this part are constructed a "compare"
function will be used to answer this question.  OCaml has the
convention of giving this compare functions the type
```
'a -> 'a -> int
```
A built in ``compare`` function can be used and has the expected
behavior.  If the two input elements are equal the function returns
``0``.  If the first is smaller than the second a negative value is
returned.  If the first is larger than the second a positive value is
returned.  Experiment with ``compare 1 2``, ``compare 4 2``, and
``compare 3 3`` in ``utop`` to understand this.


Obviously, one can write tree values that violate these invariants as
we have done above.  You must ensure that your functions *maintain*
these invariants.  That is, if all ``btree`` inputs to a function you
write satisfies these invariants, then any ``btree`` output will also
satisfy these invariants.

This means that your functions do not need to *fix* trees that violate
these invariants.  It just means that they must output trees that
satisfy the invariants when the inputs satisfy the invariants.  If the
inputs do not satisfy the invariants then your output trees do not
need to satisfy the invariants.


### Tree insertion

Write a function ``insert_by`` with the type
```
('a -> 'a -> int) -> 'a -> 'a btree -> 'a btree
```

This function takes a "compare" function as described above and adds
the second argument to the tree passed in as the third argument.

If the tree argument satisfies the invariants described above, then
your output tree must also maintain these invariants.

Add the following to the end of your ``binary_trees.ml`` file to do
some simple testing of your function.
```
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

    (* Add more asserts here as you need them *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg

```

Now add additional ``assert`` tests to more adequately test your
``insert_by`` function.


### Tree creation from a list

Now write a function ``from_list`` that adds many elements to a tree.
This function will have the type
```
('a -> 'a -> int) -> 'a list -> 'a btree
```
This function should add the elements in the list in the order in
which they appear in the list.

For example, from the list ``[4;2;5;3;6;7;8]`` your function will
first add ``4`` to the empty tree ``Nil``, then add ``2`` to the
resulting tree, and continue in this manner.

You could add the following assert to those added in the previous step
at the bottom of your file as an initial test.
```
    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   ) 
           );
```
As with the previous functions, you will want to add more tests
however. 

For full credit on this problem, use ``List.fold_left`` in a concise
and direct way to do this work.  In thinking about the functional
argument to ``fold_left`` recall the function ``flip`` that we defined
previously: 
``` 
let flip f a b = f b a 
``` 
It might be useful.  You are not required to use it, this is just
something that some solutions will find useful.


### Tree reduce and conversion to lists

We used a ``reduce`` function for the variety of trees in part 3 of
this assignment.  Now, write a ``reduce`` function for this ``btree``
type that has a similar goal as that one.  The type of this function
is up to you.

Now, you must use your ``reduce`` function to implement a function
``to_list`` with the type
```
'a btree -> 'a list
```
that extracts the elements of tree in order.

For example, for any list ``lst`` and a compare function ``cmp`` for
the elements of that list, it should be true that:
```
List.sort cmp lst = to_list (from_list cmp ls)
```

You might add the following assert to your set of tests at the end of
the file:
```
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));

```

Add some ``assert`` tests for ``reduce`` and ``to_list`` to adequately
test those two functions.

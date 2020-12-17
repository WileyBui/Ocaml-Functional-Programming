# Recursion, Lists and Higher Order Functions

## Introduction

Below, you are asked to write a number of OCaml functions.  Some are
simple, such as a function to determine if an integer input is odd or
not.  Others are more interesting, such as asking you to compute the greatest
common divisor of two positive integers.

Designing and implementing these functions will give you the
opportunity to test your knowledge of OCaml and how to write recursive
functions in it.  Successfully completing these will set you up for
the more advanced (and more interesting) topics covered in this
course.

Recall that while some labs may be done collaboratively, *this work
is meant to be done on your own.*


## Part 1 - simple functions

### even or odd 

Write an OCaml function named ``odd`` with the type ``int -> bool``
that returns ``true`` if its input is odd, ``false`` otherwise.

Recall that we used the OCaml infix operator ``mod`` in class.  You
may find it useful here.

Some example evaluations:
+ ``odd 5`` evaluates to ``true``
+ ``odd 6`` evaluates to ``false``


### Another GCD, Euclid's algorithm

In class we wrote a greatest common divisor function that computed the
GCD of two positive integers by counting down by 1 from an initial
value that was greater than or equal to the GCD until we reached a
common divisor.

You are now asked to write another GCD function that is both simpler
to write and faster.

This one is based on the following observations:
+ gcd(a,b) = a, if a = b
+ gcd(a,b) = gcd(a, b-a), if a<b
+ gcd(a,b) = gcd(a-b,b) if a>b

This function should be named ``euclid`` and have the type ``int ->
int -> int``.

To get full credit on this problem, your solution must be based on the
observations listed above.

Some example evaluations:
+ ``euclid 6 9`` evaluates to ``3``
+ ``euclid 5 9`` evaluates to ``1``


### Simplifying fractions

Write another fraction function that simplifies fractions.  It should
be called 
``frac_simplify`` with type ``(int * int) -> (int * int)``.

Consider the following sample evaluations:
+ ``frac_simplify (8,16)`` evaluates to ``(1,2)``
+ ``frac_simplify (4,9)`` evaluates to ``(4,9)``
+ ``frac_simplify (3,9)`` evaluates to ``(1,3)``

As before, you may assume that the denominator is never 0.

You may want to use your ``euclid`` function in writing
``frac_simplify``.


### Minimum in a list 

Write a function ``min_list`` that takes a list of integers as input
and returns the minimum element. 

This function should have the type ``int list -> int``.

In your solution, write a comment that specifies any restrictions on
the lists that can be passed as input to your function.  This comment
will address any aspects of this problem specification that are
incomplete.

Some sample interactions:
+ ``min_list [1; 2; 5; 3; 2]`` evaluates to ``1``
+ ``min_list [-1; -2; -5; -3; -2]`` evaluates to ``-5``


### Dropping list elements

Write another list processing function called ``drop`` with type ``int
-> 'a list -> 'a list`` that drops a specified number of elements 
from the input list.

For example, consider these evaluations:
+ ``drop 3 [1; 2; 3; 4; 5]`` evaluates to ``[4; 5]``
+ ``drop 5 ["A"; "B"; "C"]`` evaluates to ``[ ]``
+ ``drop 0 [1]`` evaluates to ``[1]``

You may assume that only non-negative numbers will be passed as the
first argument to ``drop``.


## Part 2 - higher order functions

Place your solution to these problems in a file named ``higher.ml``.
This file should not contain definitions of any recursive functions. Thus
the keyword ``rec`` may not appear in this file.

### All odds

Write a function named ``all_odds`` that returns all of the odd
numbers in a list of integers.  Use ``List.filter`` in your solution.

Your function ``all_odds`` must have the type ``int list -> int
list``.

Do not use your ``odd`` function from Part 1.  Instead write a lambda
expression as input to ``List.filter``.


### Decrement all

Write a function named ``decrement_all`` that decrements all elements
of an integer list by one.  

Your function ``decrement_all`` must have the same type as
``all_odds``.


### Min of integer list as a fold

Write another version of the ``min_list`` function from Part 1 named
``min_fold`` that uses higher order functions to compute the minimum.
It should have the same behavior as ``min_list`` but cannot be
implemented directly as a recursive function.


### Sum and prod in one pass

Write a function named ``sum_prod`` that folds up a list on integers
into a pair containing the sum and products.  The type of this
function will be ``int list -> int * int``.

For example,
+ ``sum_prod [1;2;3]`` evaluates to ``(6,6)``.
+ ``sum_prod [1; 2; 3; 4]`` evaluates to ``(10, 24)``.
+ ``sum_prod []`` evaluates to ``(0,1)``.


### Partition two ways

Write two functions, ``partition_left`` and ``partition_right`` that
each have the following type:
```
('a -> bool) -> 'a list -> 'a list * 'a list
```
These functions **must** have the same behavior - that is, on the same
inputs they return the same outputs.

Your implementation of ``partition_left`` must use ``List.fold_left``
as the higher order function that does the primary work of the
function.

Your implementation of ``partition_right`` must use ``List.fold_right``
as the higher order function that does the primary work of the
function.

For example:

- ``partition_left (fun x -> x > 4) [1;3;5;7;9;8;6;4;2]``
 
  evaluates to

  ``([5; 7; 9; 8; 6], [1; 3; 4; 2])``

- and ``partition_right (fun x -> x > 4) [1;3;5;7;9;8;6;4;2]``
 
  also evaluates to

  ``([5; 7; 9; 8; 6], [1; 3; 4; 2])``


### The generality of folds

Write a function called ``map_as_fold`` that has the same type and
behavior as ``List.map`` but has as its body a call to
``List.fold_left`` or ``List.fold_right``.

In a comment above this function, explain why you chose either 
``List.fold_left`` or ``List.fold_right``.  This explanation should
include a discussion of any difficulties you might have found in
trying to use the function you did not choose.

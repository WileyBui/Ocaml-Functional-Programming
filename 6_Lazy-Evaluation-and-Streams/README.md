# Lazy Evaluation and Streams


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


# Streams 

## Warming up with 3 versions of streams of cubes

In ``streams.ml`` write 3 functions
- ``cubes_from``
- ``cubes_from_map``
- ``cubes_from_zip``

All will have the type ``int -> int stream`` and will also compute the
same results.

- ``cubes_from`` will generate a stream of cubes, starting with the
  cube of its argument, followed by the cube of its argument plus 1,
  etc.  This version should NOT use ``map`` or ``zip`` but should
  compute its stream using a recursive call to ``cubes_from``.

- ``cubes_from_map`` must not be recursive but instead compute its
  result using the ``map`` function provided in ``streams.ml``.

- ``cubes_from_zip`` also must not be recursive but instead compute its
  result using the ``zip`` function provided in ``streams.ml``.

Below are some ``assert``s that you might use to test your solutions:
```
let () =
  assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
  assert (take 3 (cubes_from 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_zip 3) = [27; 64; 125])
```


## Factorials, reusing previous multiplication results

The provided ``streams.ml`` also contains our original definition of
``factorials``, with the small change that the built in multiplication
operator is replaced by a function `mul_p` that still multiplies its
arguments but prints out those arguments as well.

The stream ``facts`` is also defined there as ``factorials ()``.

The computation in the ``assert``, also in the file,
```
let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])
```
generates the first 5 factorials but we see in the lines printed by
``mul_p`` when the file is loaded into utop that many multiplications
are repeated.  For example, 3 is multiplied by 2 three times.

Using the techniques described in class, define ``facts'`` (note the
``'``) that computes the same stream as ``facts`` but does not repeat
these multiplications.  Specifically, the test
```
let () =
  assert (take 5 facts' = [1; 1; 2; 6; 24])
```
should produces only the following lines:
```
multiplying 1 and 1.
multiplying 2 and 1.
multiplying 3 and 2.
multiplying 4 and 6.
multiplying 5 and 24.
```

In a comment in your code after the definition of ``facts'`` explain
why ``facts'`` does not repeat the multiplications that ``facts`` does.


## The Sieve of Eratosthenes, "``sieve``"

For this final question, we ask you to write a function which computes the
[Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
There is a nice visualization of the process on this Wikipedia page
that you might watch to better understand the problem.

#### The Sieve

We can compute prime numbers by starting with the sequence (a stream) of
natural numbers beginning at 2 and using the following process:

1. Consider the first number in the sequence as a prime number.
2. Discard all multiples of the above number in the remaining sequence.
3. The remaining part of the sequence is computed by repeating this
process with the next number that is not crossed off, that is
returning to step 1.

To see this process in action, consider this prefix of natural numbers starting
at 2:
```
2 3 4 5 6 7 8 9 10 11 ...
```

Starting at step (1), we consider 2 as prime for step (2) we discard all
multiples of 2 in the remaining sequence. Notice that because 3 is not a
multiple of 2, we won't discard it:

```
3 5 7 9 11 ...
```

We then essentially "cons" 2 onto the resulting sequence beginning at 3, which
gives us this:
```
Cons (2,  3 5 7 9 11 ... )
```

For step (3), we have the resulting sequence beginning at 3 as our new sequence
and go back to step (1) to repeat the process and get this:

```
Cons (2,  Cons (3,  5 7 11 ... ))
```

You can see that we will repeat again, with 5 as the new prime and the
remaining sequence beginning at 7. This process repeats for each prime number
"discovered."

#### The ``sieve`` function

Implement the above process as a function named ``sieve``, which has type
``int stream -> int stream``. Here are some hints:

+ With your ``sieve`` and the ``from`` function from class (included
  in ``streams.ml``), you should be able
  to define the stream of prime numbers as ``let primes = sieve (from 2)``.
  Thus, the assert
  ```
  let () =
    assert ( take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] )
  ```

+ You might consider a helper ``sift`` function which implements the process we
  described above. For example, it could have type
  ``int -> int stream -> int stream`` and would output a stream which does not
  contain any multiples of its input ``int`` argument. For example, using the
  ``from`` function from ``lazy.ml``, you could have ``sift 2 (from 3)`` output
  the stream ``3 5 7 9 11 ...`` as we saw above.

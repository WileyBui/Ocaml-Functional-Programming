# Modules

## Introduction

In this assignment you will package up parts of your solution to
Homework 6 using OCaml modules to understand how write and use both
signatures and modules.  These provide an expressive way to specify
components and combine them in a principled way.

In this work you will create 4 OCaml files - named
``lazeeModules.ml``, ``streamModules.ml``, ``hwk_06_Modules.ml``, and
``hwk_07.ml`` These should be put in a directory named ``Hwk_07``.

You should read the entire specification before you start.  You may
end up developing these three files concurrently, instead of writing
them completely in order.


## Lazy modules

In class, and in homework 6, we saw types and functions that could be
used to implement a form of lazy evaluation.  You can find these in
the ``lazy.ml`` in the ``Sample Programs`` directory in the public
course repository.

In ``lazeeModules.ml`` define a signature for a lazy type using the
following code:
```ocaml
module type LazeeSig = sig
  type 'a t
  val delay: (unit -> 'a) -> 'a t
  val demand: 'a t -> 'a 
end
```
We see that ``t`` is an opaque type constructor accompanied by two
functions for creating and using values of this type.  Compare these
types to the corresponding types in ``lazy.ml``.

Your first task is to define a module ``Lazee_v1`` (in ``lazeeModules.ml`` 
that implements the
signature ``LazeeSig`` following the definition of lazy values from
``lazy.ml``.  This can be done by filling in the following incomplete
module.  Note that your module may define values that are not in the
``LazeeSig`` signature and thus not used outside of the ``Lazee_v1``
module.

```ocaml
module Lazee_v1 : LazeeSig = struct

end
```

We can also write a module satisfying this signature that does not use
references and save the value of executing a ``Thunk`` in a ``Value``
constructor so that these functions are only run once.  This is done
in the following implementation which you should copy into the
``lazeeModules.ml`` file.
```ocaml
module Lazee_v2 : LazeeSig = struct
  type 'a t = unit -> 'a
  let delay (unit_to_x: unit -> 'a) : 'a t = unit_to_x
  let demand (l: 'a t) : 'a = l ()
end
```

Thus ``lazeeModules.ml`` will have one signature and two modules defined in it. 


## A stream functor

Streams, as defined in the ``lazy.ml`` file mentioned above, made use
of the ``lazee`` type and its functions defined in that same file.
Here we want to make this dependency explicit by defining a stream
functor that takes a lazy module as it argument.

To do this, you are asked to complete the following stream signature
and functor in the file named ``streamModules.ml``.  Note that both of 
these are incomplete and you need to add elements to both of them. 
```ocaml
module type StreamSig = sig
  type 'a lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 

  type 'a t = Cons of 'a * 'a t lazee

  (* Add more elements here. *)
end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  let delay = L.delay
  let demand = L.demand

  (* add more elements here *)
end
```

Notice that a stream module created by the ``Streams`` functor will
need to expose some aspects of the lazy module they are parameterized
by.  This is because some of our stream functions, such as ``filter``
needed the ``delay`` function to construct the stream.

The ``Stream`` functor should provide implementations of the
functions ``head``, ``tail``, ``take``, ``filter``, ``map``, and
``zip``.  We have seen implementations of these before so it is just a
matter of packaging them up in a module.  Part of this work involves small changes to their previous implementation to access the elements of the module ``L``.



Since we have two modules that implement a lazy type, we can construct
two different stream implementations using our stream functor.  This
can be done with the following to module specifications.  These will go into the ``hwk_07.ml`` file where we use the functors defined in the other files to construct the modules you'll need there.

```ocaml
module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)
```

## Homework 6 - again, but using modules

For this part of the assignment you are asked to write a functor named
``Hwk_06`` that takes a module implementing the ``StreamSig``
signature.  This functor will be sealed by the ``Hwk_06_Sig`` signature.
This signature, and the incomplete functor are shown below:
```ocaml
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
   (* add elements here to complete the functor *)
end
```

Place these definitions into the file named ``hwk_06_Modules.ml``.
Many of the functions above should be familiar to you.  You
implemented them in Homework 6.  Here, just package those solutions in
the functor, making any required changes in doing so.  It may not be
sufficient to simply copy-and-paste those old solutions into the
functor.


## Testing your code.

Create a file named ``hwk_07.ml`` and add the following OCaml code.

Compiling this file, using ``ocamlbuild`` will let you test your
code.  When running it, you should see the final message "Success, all
tests passed." appear.  (You can also test your code in ``utop`` by
appropriate use of ``#mod_use`` and ``#use``.)

```ocaml
open LazeeModules
open StreamModules
open Hwk_06_Modules

module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)

module Hwk_06_Lazy = Hwk_06( Stream_Lazy )
module Hwk_06_Slow = Hwk_06( Stream_Slow )

module Hwk_07_Test (Hwk_06: Hwk_06_Sig) = struct
  let () =
    assert (Hwk_06.take 4 (Hwk_06.nats) = [1; 2; 3; 4]);
    assert (Hwk_06.take 10 (Hwk_06.nats) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from 1) = [1; 8; 27; 64]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from_zip 1) = [1; 8; 27; 64]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from_map 1) = [1; 8; 27; 64]);

    assert (Hwk_06.take 5 Hwk_06.facts = [1; 1; 2; 6; 24]);
    assert (Hwk_06.take 5 Hwk_06.facts' = [1; 1; 2; 6; 24]);

    assert (Hwk_06.take 10 Hwk_06.primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29])
end

module Test_Lazy = Hwk_07_Test(Hwk_06_Lazy)
module Test_Slow = Hwk_07_Test(Hwk_06_Slow)

let () =
  print_endline ("Success, all tests passed.")
```

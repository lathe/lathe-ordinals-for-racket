#lang parendown scribble/manual

@; lathe-ordinals/scribblings/lathe-ordinals.scrbl
@;
@; Ordinal number utilities.

@;   Copyright 2018 The Lathe Authors
@;
@;   Licensed under the Apache License, Version 2.0 (the "License");
@;   you may not use this file except in compliance with the License.
@;   You may obtain a copy of the License at
@;
@;       http://www.apache.org/licenses/LICENSE-2.0
@;
@;   Unless required by applicable law or agreed to in writing,
@;   software distributed under the License is distributed on an
@;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
@;   either express or implied. See the License for the specific
@;   language governing permissions and limitations under the License.


@(require #/for-label racket/base)
@(require #/for-label #/only-in racket/contract/base
  *list/c -> any/c flat-contract? list/c listof or/c)
@(require #/for-label #/only-in racket/math natural?)

@(require #/for-label #/only-in lathe-comforts/maybe maybe?)

@(require #/for-label lathe-ordinals)
@(require #/for-label lathe-ordinals/olist)

@(require #/only-in scribble/example examples make-eval-factory)

@(define example-eval
  (make-eval-factory #/list 'racket/base 'lathe-ordinals))


@title{Lathe Ordinals}

Lathe Ordinals for Racket provides data structures and utilities for managing ordinal numbers. Ordinal numbers are good for representing things that go on forever, and then some, and then keep going on forever again... and then a little more....

For instance, if you have some relational data with two numeric columns in it, then you might want to consider that pair of numbers to be ordered on those columns according to the progression @tt{(0, 0)}, @tt{(0, 1)}, @tt{(0, 2)}, and so on, then @tt{(1, 0)}, @tt{(1, 1)}, @tt{(1, 2)}, and so on, then @tt{(2, 0)}, @tt{(2, 1)}, @tt{(2, 2)}, and so on, and so on. (A lexicographic ordering.) If that sounds familiar, omega squared may be the ordinal for you.

Here's how to count all the ordinals less than omega-squared: @tt{0}, @tt{1}, @tt{2}, and so on, then @tt{omega}, @tt{(omega + 1)}, @tt{(omega + 2)}, and so on, then @tt{(omega * 2)}, @tt{((omega * 2) + 1)}, @tt{((omega * 2) + 2)}, and so on, and so on. This progression corresponds to all those number pairs. The first ordinal that doesn't correspond to any of them is the next one, @tt{(omega ^ 2)} itself.

The larger the ordinals get, the more challenging they are to completely model. In fact, one of the ways people study the relative strengh of mathematical foundations is by how large an ordinal each of those foundations can deal with (ordinal analysis). This library will never be able to explore all of them, and at the moment, this library reaches its limit at epsilon zero.

There are some slight @emph{variations} on ordinals that make sense in slight variations of set theory, such as the constructive ordinals of constructive set theory. This library might never explore all of those either, and for now it's concerned exclusively with the standard notion of ordinal familiar from ZFC set theory.

That's a lot of talk about set theory and mathematical foundations, but this library is primarily intended for use as a number system for programmers to use, like libraries for floating point numbers or matrices.

@; TODO: This is a good introduction to how ordinals work and what our
@; algorithms are doing, and we should find a good place to link to
@; it:
@;
@; https://www.maplesoft.com/products/maple/new_features/maple19/Ordinals_Maple2015.pdf



@table-of-contents[]



@section[#:tag "onum"]{Ordinal numerals}

@defmodule[lathe-ordinals]

This module provides data structures to represent the ordinal numbers of ZFC set theory that are less than or equal to epsilon zero. The ordinals less than epsilon zero are the earliest set of ordinals that contains omega and has full support for ordinal addition, multiplication, and exponentiation.

All the ordinals of this library can be compared using `equal?`. Finite ordinals are represented using natural numbers (@racket[natural?]).


@defproc[(onum<=e0? [v any/c]) boolean?]{
  Returns whether the given value is an ordinal number constructed by this library. All such ordinals are less than or equal to epsilon zero.
}

@defproc[(onum<e0? [v any/c]) boolean?]{
  Returns whether the given value is an ordinal number strictly less than epsilon zero. These are the ordinal numbers that most of the arithmetic operations of this library are restricted to.
}

@defproc[(onum->cnf [n onum<e0?]) (listof (list/c onum<e0? exact-positive-integer?))]{
  Given an ordinal less than epsilon zero, returns a list of the powers and coefficients of the ordinal in Cantor normal form.
  
  @examples[
    #:eval (example-eval)
    (define _omega-plus-four (onum-plus (onum-omega) 4))
    _omega-plus-four
    (onum->cnf _omega-plus-four)
    (define _square-of-that
      (onum-times _omega-plus-four _omega-plus-four))
    _square-of-that
    (onum->cnf _square-of-that)
  ]
}

@defproc[(onum-compare [a onum<=e0?] [b onum<=e0?]) (or/c '< '= '>)]{
  Returns the symbol @racket['<] if the numbers are provided in ascending order, @racket['=] if they're equal, and @racket['>] if they're in descending order.
}

@defproc[(onum</c [n onum<=e0?]) flat-contract?]{
  Returns a flat contract that requires the input to be an ordinal number strictly less than @racket[n].
}

@defproc[(onum-omega) onum<e0?]{
  Returns the ordinal number omega. Every ordinal less than omega is a natural number.
}

@defproc[(onum-e0) onum<=e0?]{
  Returns the ordinal number epsilon zero, the first infinite ordinal that can't be arrived at by exponentiation, multiplication, and addition.
}

@defproc[(nat->onum [n natural?]) onum?]{
  Returns an ordinal number corresponding to the given natural number.
}

@defproc*[(
  [ (onum-plus-list [ns (or/c (list/c) (*list/c onum<e0? onum<=e0?))])
    onum<=e0?]
  [(onum-plus [a onum<e0?] ... [b onum<=e0?]) onum<=e0?]
)]{
  Adds up the given ordered list of ordinal numbers.
}

@defproc*[(
  [
    (onum-times-list
      [ns (or/c (list/c) (*list/c onum<e0? onum<=e0?))])
    onum<=e0?]
  [(onum-times [a onum<e0?] ... [b onum<=e0?]) onum<=e0?]
)]{
  Multiplies the given ordered list of ordinal numbers.
}

@; TODO: Document this module's other exports.



@section[#:tag "olist"]{Ordinal-indexed lists}

@defmodule[lathe-ordinals/olist]

This module provides a lazy list data structure that can have length less than or equal to epsilon zero. Most of what you can do with these lazy lists, you can do just by writing a function that takes an ordinal number as its argument. The difference has to do with garbage collection: These lists are designed so that if you append and remove an element, you can be sure that the diminished list no longer contains a reference to it.


@defproc[(olist<=e0? [v any/c]) boolean?]{
  Returns whether the given value is an ordinal-indexed list constructed by this library. Every such list has a length no greater than epsilon zero.
}

@defproc[(olist<e0? [v any/c]) boolean?]{
  Returns whether the given value is an ordinal-indexed list with length strictly less than epsilon zero.
}

@defproc[(olist-zero) olist<e0?]{
  Returns an empty ordinal-indexed list.
}

@defproc[
  (olist-build
    [len onum<=e0?]
    [index->element (-> (onum</c len) any/c)])
  olist<=e0?
]{
  Returns an ordinal-indexed list of the given length, which computes each element by passing the element's index to the given procedure.
}

@; TODO: Document this module's other exports.

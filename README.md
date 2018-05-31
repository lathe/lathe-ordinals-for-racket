# Lathe Ordinals for Racket

[![Travis build](https://travis-ci.org/lathe/lathe-ordinals-for-racket.svg?branch=master)](https://travis-ci.org/lathe/lathe-ordinals-for-racket)

Lathe Ordinals for Racket provides data structures and utilities for managing ordinal numbers. Ordinal numbers are good for representing things that go on forever, and then some, and then keep going on forever again... and then a little more....

For instance, if you have some relational data with two numeric columns in it, then you might want to consider that pair of numbers to be ordered on those columns according to the progression `(0, 0)`, `(0, 1)`, `(0, 2)`, and so on, then `(1, 0)`, `(1, 1)`, `(1, 2)`, and so on, then `(2, 0)`, `(2, 1)`, `(2, 2)`, and so on, and so on. (A lexicographic ordering.) If that sounds familiar, omega squared may be the ordinal for you.

Here's how to count all the ordinals less than omega-squared: `0`, `1`, `2`, and so on, then `omega`, `(omega + 1)`, `(omega + 2)`, and so on, then `(omega * 2)`, `((omega * 2) + 1)`, `((omega * 2) + 2)`, and so on, and so on. This progression corresponds to all those number pairs. The first ordinal that doesn't correspond to any of them is the next one, `(omega ^ 2)` itself.

The larger the ordinals get, the more challenging they are to completely model. In fact, one of the ways people study the relative strengh of mathematical foundations is by how large an ordinal each of those foundations can deal with (ordinal analysis). This library will never be able to explore all of them, and at the moment, this library reaches its limit at epsilon zero.

There are some slight _variations_ on ordinals that make sense in slight variations of set theory, such as the constructive ordinals of constructive set theory. This library might never explore all of those either, and for now it's concerned exclusively with the standard notion of ordinal familiar from ZFC set theory.

That's a lot of talk about set theory and mathematical foundations, but this library is primarily intended for use as a number system for programmers to use, like libraries for floating point numbers or matrices.

Here's what the library offers now:

* Data structures to represent the ordinal numbers of ZFC set theory that are less than or equal to epsilon zero (`onum<=e0?`). The ordinals less than epsilon zero (`onum<e0?`) are the earliest set of ordinals that contains omega and has full support for ordinal addition, multiplication, and exponentiation.

* A corresponding data structure (`olist<=e0?`) to implement lazy lists of length less than or equal to epsilon zero. Most of what you can do with these, you can do just by writing a function that takes an ordinal number as its argument. The difference has to do with garbage collection: These lists are designed so that if you append and remove an element, you can be sure that the diminished list no longer contains a reference to it.


## Installation and use

This is a library for Racket. To install it from the Racket package index, run `raco pkg install lathe-ordinals`. Then you can put an import like `(require lathe-comforts)` in your Racket program.

To install it from source, run `raco pkg install --deps search-auto` from the `lathe-ordinals-lib/` directory.

[Documentation for Lathe Ordinals for Racket](http://docs.racket-lang.org/lathe-ordinals/index.html) is available at the Racket documentation website, and it's maintained in the `lathe-comforts-doc/` directory.

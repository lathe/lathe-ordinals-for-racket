#lang parendown racket/base

; lathe-ordinals/olist-codata
;
; A stream-like collection where the indexes can be any ordinal number
; less than epsilon zero. These are like the ordinal-indexed lists of
; the `lathe-ordinals/olist` module, but they're designed to be
; potentially unbounded by any ordinal, and if they are bounded, their
; length may not be known right away.
;
; This is designed to represent the codata type that's the categorical
; dual of the data represented by the ordinal-indexed lists of the
; `lathe-ordinals/olist` module.
;
; We also supply a dual for ordinal numbers themselves, which we call
; "ordinal numeral computations." They're isomorphic to
; ordinal-indexed streams with trivial elements. The length of an
; ordinal-indexed stream (`olist-codata...`) is an ordinal numeral
; computation (`onum-codata...`).
;
; Our representation uses ordinal-sized "batches" so that we can read
; infinite-ordinal-sized chunks of data from a stream in a program
; that still terminates. We provide these batches as data structures
; of their own (`onum-batch...` and `ostream-batch...`). They could be
; useful for implementing other stream transformations.

;   Copyright 2018 The Lathe Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.



(require #/only-in racket/contract/base
  -> any/c contract? contract-out list/c or/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn loopfn mat w- w-loop)
(require #/only-in lathe-comforts/contract fix/c)
(require #/only-in lathe-comforts/match match/c)
(require #/only-in lathe-comforts/maybe
  just maybe-bind maybe/c nothing)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in lathe-ordinals
  onum-compare onum-drop onum-e0 onum<e0? 0<onum<e0?
  onum<=greatest-known? onum-omega onum-plus onum-pow)
(require #/only-in lathe-ordinals/olist
  olist-build olist-drop olist-drop1 olist<e0? 0<olist<e0?
  olist<=greatest-known? olist-length olist-map olist-map-kv
  olist-plus olist-ref-thunk olist-tails olist-transfinite-unfold
  olist-zero olist-zip-map)

; TODO: Document all of these exports.
(provide

  onum-batch?
  onum-batch-with-progress?
  onum-batch-done
  onum-batch-stuck
  onum-batch-plus-onum
  onum-batch-plus1
  onum-batch-drop
  onum-batch-drop1
  onum-batch-proceed

  olist-batch?
  olist-batch-with-progress?
  olist-batch-length
  olist-batch-done
  olist-batch-stuck
  olist-batch-plus-olist
  olist-batch-plus1
  olist-batch-drop
  olist-batch-drop1
  olist-batch-proceed
  
  (contract-out
    [onum-codata? (-> any/c boolean?)])
  onum-batches/c
  onum-batches->onum-codata
  onum->onum-codata
  onum-codata-unknowable
  onum-codata-plus1
  onum-codata-plus-onum
  onum-codata-drop1
  onum-codata-drop
  
  (contract-out
    [olist-codata? (-> any/c boolean?)])
  olist-batches/c
  olist-batches->olist-codata
  olist->olist-codata olist-codata-build olist-codata-length
  olist-codata-plus1
  olist-codata-plus-olist
  olist-codata-drop1
  olist-codata-drop
  
  ; TODO: See if we can define operations analogous to these, for each
  ; of `onum-batch...`, `olist-batch...`, `onum-codata...`, and
  ; `olist-codata...`.
  ;
;  onum-times-list onum-times
;  onum-untimes
;  onum->limit-plus-finite
;  onum-pred-maybe
;  onum-pow-list onum-pow
  ;
  ; TODO: If we ever implement an `onum-log`, consider its analogous
  ; operations here as as well.
  
  ; TODO: See if we'll ever use `olist-codata-map-kv` -- or any of the
  ; others, for that matter.
  olist-codata-map olist-codata-map-kv olist-codata-zip-map
  olist-codata-tails
  olist-codata-transfinite-unfold-forever
  
  olist-codata-ref-maybe-thunk
)



(define-imitation-simple-struct
  (onum-batch-rep-done? onum-batch-rep-done-onum)
  onum-batch-rep-done
  'onum-batch-rep-done (current-inspector) (auto-write))
(define-imitation-simple-struct
  (onum-batch-rep-stuck? onum-batch-rep-stuck-effect)
  onum-batch-rep-stuck
  'onum-batch-rep-stuck (current-inspector) (auto-write))
; NOTE: The `onum-batch-rep-plus-onum` argument should not be `0`. The
; `onum-batch-rep-plus-rest` argument should not be an
; `onum-batch-rep-done` or `onum-batch-rep-plus` value. This makes it
; easy to check whether the batch is stuck or zero.
(define-imitation-simple-struct
  (onum-batch-rep-plus?
    onum-batch-rep-plus-onum onum-batch-rep-plus-rest)
  onum-batch-rep-plus
  'onum-batch-rep-plus (current-inspector) (auto-write))

(define/contract (onum-batch? v)
  (-> any/c boolean?)
  (or
    (onum-batch-rep-done? v)
    (onum-batch-rep-stuck? v)
    (onum-batch-rep-plus? v)))

(define/contract (onum-batch-with-progress? v)
  (-> any/c boolean?)
  (and (onum-batch? v)
  #/mat v (onum-batch-rep-done v) #t
  #/mat v (onum-batch-rep-stuck effect) #f
  #/dissect v (onum-batch-rep-plus a b) #t))

(define/contract (onum-batch-done n)
  (-> onum<e0? onum-batch-with-progress?)
  (onum-batch-rep-done n))

(define/contract (onum-batch-stuck effect)
  (-> any/c onum-batch?)
  (onum-batch-rep-stuck effect))

; NOTE:
;
; We don't allow batches to be added to batches. To append information
; to the leaves of a tree data structure, we usually have to dig down
; to the leaves and append it there. An `onum-batch` is supposed to be
; a generalization of a unary number representation, so in order to
; add onto the end of it, it's necessary to traverse it first. A stuck
; batch can represent an ordinal number that we haven't even decided
; *how* to traverse yet (as in, deciding how big a chunk we should try
; to drop from it first).
;
; Deciding on a computation strategy that will traverse the whole
; thing is like picking a sequence of ordinals we can use to postulate
; the existence of an ordinal at their limit. If we haven't picked the
; things we need to obtain the first ordinal we're adding, we can't
; very well even *begin* to traverse successors and limits beyond that
; to get to the sum we're computing.
;
; If we nevertheless tried to implement a system that allowed batches
; to be added to batches, things would get tricky fast. We'd have to
; deal with operations like dropping 3 from
; `(onum-batch-plus (onum-batch-stuck 'foo) (onum-batch-done 100))`.
; We can deduce this batch is at least as great as 100, so it should
; be possible to drop 3, right? Should we end up with a batch that
; represents "if `'foo` is finite, then `'foo` plus 97; otherwise
; `'foo` plus 100"? If we did that, then once we got to doing the
; corresponding thing for `olist-batch`, we'd have to represent a
; dropped value of "if `'foo` has a length of 3 or greater, then the
; first 3 elements of `'foo`, else all the elements of `'foo` plus the
; first (3 - length of `'foo) elements of the 100-element `olist`."
; That means we traverse part of this codata and we don't even get
; data back, just more codata, (All right, the same could be said of
; dropping infinite-ordinal-sized chunks from a stream, so maybe it's
; fine.) Now we have to consider how *these* batches add together and
; what we can deduce about them to do dropping operations. The full
; scope of the effort this would entail isn't clear yet, and it isn't
; clear it's worth it.
;
(define/contract (onum-batch-plus-onum amount n)
  (-> onum<e0? onum-batch? onum-batch?)
  
  ; We don't use `onum-batch-rep-plus` if either argument is known to
  ; be zero.
  (mat amount 0 n
  
  #/mat n (onum-batch-rep-done n)
    (onum-batch-rep-done #/onum-plus amount n)
  #/mat n (onum-batch-rep-stuck effect)
    (onum-batch-rep-plus amount n)
  #/dissect n (onum-batch-rep-plus a b)
    (onum-batch-rep-plus (onum-plus amount a) b)))

(define/contract (onum-batch-plus1 n)
  (-> onum-batch? onum-batch?)
  (onum-batch-plus-onum 1 n))

(define/contract (onum-batch-drop on-stuck)
  (->
    (->
      onum-batch?
      onum-batch?
      (-> any/c)
      (-> onum-batch? any/c)
      (-> onum-batch? any/c)
      any/c)
    (->
      onum-batch?
      onum-batch?
      (-> any/c)
      (-> onum-batch? any/c)
      (-> onum-batch? any/c)
      any/c))
  (loopfn recur a b on-both-zero on-a-zero on-b-zero
    (if
      (not #/and
        (onum-batch-with-progress? a)
        (onum-batch-with-progress? b))
      (on-stuck a b on-a-zero on-b-zero)
    #/mat a (onum-batch-rep-plus aa ab)
      (recur (onum-batch-rep-done aa) b
        (fn #/on-b-zero ab)
        (fn b #/recur ab b on-both-zero on-a-zero on-b-zero)
        (dissectfn (onum-batch-rep-done aa)
          (on-b-zero #/onum-batch-plus-onum aa ab)))
    #/mat b (onum-batch-rep-plus ba bb)
      (recur a (onum-batch-rep-done ba)
        (fn #/on-a-zero bb)
        (dissectfn (onum-batch-rep-done ba)
          (on-a-zero #/onum-batch-plus-onum ba bb))
        (fn a #/recur a bb on-both-zero on-a-zero on-b-zero))
    #/dissect a (onum-batch-rep-done a)
    #/dissect b (onum-batch-rep-done b)
    #/mat (onum-drop a b) (just b)
      (mat b 0
        (on-both-zero)
        (on-a-zero #/onum-batch-rep-done b))
    #/dissect (onum-drop b a) (just a)
      (on-b-zero #/onum-batch-rep-done a))))

(define/contract (onum-batch-drop1 on-stuck)
  (->
    (->
      onum-batch?
      (-> any/c)
      (-> onum-batch? any/c)
      (-> any/c)
      any/c)
    (->
      onum-batch?
      (-> any/c)
      (-> onum-batch? any/c)
      (-> any/c)
      any/c))
  (w- drop
    (onum-batch-drop #/fn one b on-both-zero on-1-zero on-b-zero
      (on-stuck b on-both-zero on-1-zero (fn #/on-b-zero one)))
  #/fn b on-both-zero on-1-zero on-b-zero
    (drop (onum-batch-rep-done 1) b
      on-both-zero
      on-1-zero
      (fn one-zero #/on-b-zero))))

(define/contract (onum-batch-proceed effect-proceed)
  (-> (-> any/c onum-batch?) (-> onum-batch? onum-batch?))
  (loopfn recur n
    (mat n (onum-batch-rep-done _)
      n
    #/mat n (onum-batch-rep-stuck effect)
      (effect-proceed effect)
    #/dissect n (onum-batch-rep-plus a b)
      (onum-batch-rep-plus a (recur b)))))



(define-imitation-simple-struct
  (olist-batch-rep-done? olist-batch-rep-done-olist)
  olist-batch-rep-done
  'olist-batch-rep-done (current-inspector) (auto-write))
(define-imitation-simple-struct
  (olist-batch-rep-stuck? olist-batch-rep-stuck-effect)
  olist-batch-rep-stuck
  'olist-batch-rep-stuck (current-inspector) (auto-write))
; NOTE: The `olist-batch-rep-plus-olist` argument should not have
; length `0`. The `olist-batch-rep-plus-rest` argument should not be
; an `olist-batch-rep-done` or `olist-batch-rep-plus` value. This
; makes it easy to check whether the batch is stuck or zero.
(define-imitation-simple-struct
  (olist-batch-rep-plus?
    olist-batch-rep-plus-olist olist-batch-rep-plus-rest)
  olist-batch-rep-plus
  'olist-batch-rep-plus (current-inspector) (auto-write))

(define/contract (olist-batch? v)
  (-> any/c boolean?)
  (or
    (olist-batch-rep-done? v)
    (olist-batch-rep-stuck? v)
    (olist-batch-rep-plus? v)))

(define/contract (olist-batch-with-progress? v)
  (-> any/c boolean?)
  (and (olist-batch? v)
  #/mat v (olist-batch-rep-done v) #t
  #/mat v (olist-batch-rep-stuck effect) #f
  #/dissect v (olist-batch-rep-plus a b) #t))

(define/contract (olist-batch-done n)
  (-> olist<e0? olist-batch-with-progress?)
  (olist-batch-rep-done n))

(define/contract (olist-batch-stuck effect)
  (-> any/c olist-batch?)
  (olist-batch-rep-stuck effect))

(define/contract (olist-batch-length effect-length)
  (-> (-> any/c onum-batch?) (-> olist-batch? onum-batch?))
  (loopfn recur n
    (mat n (olist-batch-rep-done n)
      (onum-batch-rep-done #/olist-length n)
    #/mat n (olist-batch-rep-stuck effect)
      (effect-length effect)
    #/dissect n (olist-batch-rep-plus a b)
      (onum-batch-rep-plus (olist-length a) (recur b)))))

; NOTE: See the note at `onum-batch-plus-onum` for the reason this
; doesn't let us add two batches.
(define/contract (olist-batch-plus-olist amount n)
  (-> olist<e0? olist-batch? olist-batch?)
  
  ; We don't use `olist-batch-rep-plus` if either argument is known to
  ; be empty.
  (mat (olist-length amount) 0 n
  
  #/mat n (olist-batch-rep-done n)
    (olist-batch-rep-done #/olist-plus amount n)
  #/mat n (olist-batch-rep-stuck effect)
    (olist-batch-rep-plus amount n)
  #/dissect n (olist-batch-rep-plus a b)
    (olist-batch-rep-plus (olist-plus amount a) b)))

(define/contract (olist-batch-plus1 get-first n)
  (-> (-> any/c) olist-batch? olist-batch?)
  (olist-batch-plus-olist (olist-build 1 #/dissectfn _ #/get-first)
    n))

(define/contract (olist-batch-drop on-stuck)
  (->
    (->
      olist-batch?
      olist-batch?
      (-> any/c)
      (-> olist<=greatest-known? olist-batch? any/c)
      (-> olist<=greatest-known? olist-batch? any/c)
      any/c)
    (->
      olist-batch?
      olist-batch?
      (-> any/c)
      (-> olist<=greatest-known? olist-batch? any/c)
      (-> olist<=greatest-known? olist-batch? any/c)
      any/c))
  (loopfn recur a b on-both-zero on-a-zero on-b-zero
    (if
      (not #/and
        (olist-batch-with-progress? a)
        (olist-batch-with-progress? b))
      (on-stuck a b on-a-zero on-b-zero)
    #/mat a (olist-batch-rep-plus aa ab)
      (recur (olist-batch-rep-done aa) b
        (fn #/on-b-zero aa ab)
        (fn b-dropped b-rest
          (recur ab b-rest
            on-both-zero
            (fn b-dropped-2 b-rest
              (on-a-zero (olist-plus b-dropped b-dropped-2) b-rest))
            (fn ab-dropped ab-rest
              (on-b-zero (olist-plus aa ab-dropped) ab-rest))))
        (fn aa-dropped aa-rest
          (dissect aa-rest (olist-batch-rep-done aa-rest)
          #/on-b-zero
            aa-dropped
            (olist-batch-plus-olist aa-rest ab))))
    #/mat b (olist-batch-rep-plus ba bb)
      (recur a (olist-batch-rep-done ba)
        (fn #/on-a-zero ba bb)
        (fn ba-dropped ba-rest
          (dissect ba-rest (olist-batch-rep-done ba-rest)
          #/on-a-zero
            ba-dropped
            (olist-batch-plus-olist ba-rest bb)))
        (fn a-dropped a-rest
          (recur a-rest bb
            on-both-zero
            (fn bb-dropped bb-rest
              (on-a-zero (olist-plus ba bb-dropped) bb-rest))
            (fn a-dropped-2 a-rest
              (on-b-zero (olist-plus a-dropped a-dropped-2)
                a-rest)))))
    #/dissect a (olist-batch-rep-done a)
    #/dissect b (olist-batch-rep-done b)
    #/mat (olist-drop (olist-length a) b) (just b-dropped-and-rest)
      (dissect b-dropped-and-rest (list b-dropped b-rest)
      #/mat (olist-length b-rest) 0
        (on-both-zero)
        (on-a-zero b-dropped #/olist-batch-rep-done b-rest))
    #/dissect (olist-drop b a) (just a-dropped-and-rest)
      (dissect a-dropped-and-rest (list a-dropped a-rest)
      #/on-b-zero a-dropped #/olist-batch-rep-done a-rest))))

(define/contract (olist-batch-drop1 get-first on-stuck)
  (->
    (-> any/c)
    (->
      olist-batch?
      (-> any/c)
      (-> olist-batch? any/c)
      (-> any/c)
      any/c)
    (->
      olist-batch?
      (-> any/c)
      (-> olist-batch? any/c)
      (-> any/c)
      any/c))
  (w- drop
    (olist-batch-drop #/fn one b on-both-zero on-1-zero on-b-zero
      (on-stuck b on-both-zero on-1-zero (fn #/on-b-zero one)))
  #/fn b on-both-zero on-1-zero on-b-zero
    (drop
      (olist-batch-rep-done #/olist-build 1 #/dissectfn _ #/get-first)
      b
      on-both-zero
      on-1-zero
      (fn one-zero #/on-b-zero))))

(define/contract (olist-batch-proceed effect-proceed)
  (-> (-> any/c olist-batch?) (-> olist-batch? olist-batch?))
  (loopfn recur n
    (mat n (olist-batch-rep-done _)
      n
    #/mat n (olist-batch-rep-stuck effect)
      (effect-proceed effect)
    #/dissect n (olist-batch-rep-plus a b)
      (olist-batch-rep-plus a (recur b)))))



(define-imitation-simple-struct (onum-codata? onum-codata-get-batch)
  onum-codata 'onum-codata (current-inspector) (auto-write))

(define/contract (onum-batches/c)
  (-> contract?)
  (fix/c onum-batches/c #/-> #/or/c
    (match/c onum-batch-rep-done onum<e0?)
    (match/c onum-batch-rep-stuck onum-batches/c)
    (match/c onum-batch-rep-plus 0<onum<e0?
      (match/c onum-batch-rep-stuck onum-batches/c))))

; Given a thunk which contains an ordinal numeral batch, where the
; `effect` of the batch is another such thunk, this returns an ordinal
; numeral computation that follows all those thunks.
;
; If the batches are actually infinite in number, it's recommended to
; have each batch increase in size to approach epsilon zero or another
; large-enough ordinal for your purposes. This is because if the
; maximum batch size doesn't exceed some bound N, then the act of
; calling `onum-codata-drop` with an amount of (N * omega) will cause
; the program to go into an infinite loop trying to compute all the
; batches.
;
; TODO: The same will be true for `onum-codata-untimes` and
; `onum-codata-log`, if and when we have those, at which point we'll
; want to encourage growth of this style, for increasing `big-onum`
; values:
;
;   (onum-batch-plus-onum big-onum
;   #/onum-batch-times-onum big-onum
;   #/onum-batch-pow-onum big-onum
;     ...)
;
(define/contract (onum-batches->onum-codata batches)
  (-> (onum-batches/c) onum-codata?)
  (onum-codata batches))

(define/contract (onum-codata-unknowable)
  (-> onum-codata?)
  (onum-codata
    (w-loop next big-onum (onum-omega)
      (fn
        ; TODO: If we ever have `onum-batch-times-onum` and
        ; `onum-batch-pow-onum`, we should use them here like so:
        ;
        ;   (onum-batch-plus-onum big-onum
        ;   #/onum-batch-times-onum big-onum
        ;   #/onum-batch-pow-onum big-onum
        ;     ...)
        ;
        ; That way, untimes and log operations will terminate.
        ;
        (onum-batch-plus-onum big-onum
        #/onum-batch-stuck
        #/next #/onum-pow (onum-omega) big-onum)))))

(define/contract (onum->onum-codata n)
  (-> onum<=greatest-known? onum-codata?)
  (if (onum<e0? n) (onum-codata #/fn #/onum-batch-done n)
  
  ; NOTE:
  ;
  ; We allow epsilon zero to be converted to an ordinal numeral
  ; computation, but we don't allow it to be fully traversed (with
  ; `onum-codata-drop`). This is because we have some things like
  ; `(onum-codata-unknowable)` which are *designed* to be larger than
  ; any known ordinal. If a programmer consumes data from a single
  ; ordinal-indexed stream (or in this case an ordinal numeral
  ; computation), they will often want to collect that data onto an
  ; ordinal-indexed list as they go along (or in this case onto an
  ; ordinal numeral). The data structures we offer can't go deeper
  ; than epsilon zero, so if the programmer could reach a depth of
  ; epsilon zero and find that they still have further to go, the
  ; data structure they're aggregating things into would run out of
  ; space.
  ;
  ; This sort of means our ordinal numerals and ordinal-indexed lists
  ; can exceed the size of our ordinal numeral computations and
  ; ordinal-indexed-streams, in a certain sense, just slightly. But if
  ; this library ever gains support for ordinal numbers larger than
  ; epsilon zero, some preexisting uses of ordinal-indexed streams
  ; might beome larger in fact than any preexisting use of
  ; ordinal-indexed lists.
  ;
  #/onum-codata-unknowable))

(define/contract (onum-codata-plus1 n)
  (-> onum-codata? onum-codata?)
  (dissect n (onum-codata get-batch)
  #/onum-codata #/fn #/onum-batch-plus1 #/get-batch))

(define/contract (onum-codata-plus-onum amount n)
  (-> onum<e0? onum-codata? onum-codata?)
  (dissect n (onum-codata get-batch)
  #/onum-codata #/fn #/onum-batch-plus-onum amount #/get-batch))

(define/contract (onum-codata-drop1 n)
  (-> onum-codata? #/maybe/c onum-codata?)
  (dissect n (onum-codata get-batch)
  #/w- drop
    (w-loop drop []
      (onum-batch-drop1 #/fn b on-both-zero on-1-zero on-b-zero
        (dissect b (onum-batch-rep-stuck get-batch)
        #/drop (get-batch) on-both-zero on-1-zero on-b-zero)))
  #/drop (get-batch)
    (fn #/just #/onum->onum-codata 0)
    (fn result #/just result)
    (fn #/nothing)))

(define/contract (onum-codata-drop amount n)
  (-> onum-codata? onum-codata? #/maybe/c onum-codata?)
  (dissect amount (onum-codata amount-get-batch)
  #/dissect n (onum-codata n-get-batch)
  #/w- drop
    (w-loop drop []
      (onum-codata-drop
      #/fn amount n on-both-zero on-amount-zero on-n-zero
        (w- unstick
          (fn n
            (mat n (onum-batch-rep-stuck n-get-batch)
              (n-get-batch)
              n))
        #/drop (unstick amount) (unstick n)
          on-both-zero on-amount-zero on-n-zero)))
  #/drop (amount-get-batch) (n-get-batch)
    (fn #/just #/onum->onum-codata 0)
    (fn result #/just result)
    (fn remainder #/nothing)))



(define-imitation-simple-struct (olist-codata? olist-codata-get-batch)
  olist-codata 'olist-codata (current-inspector) (auto-write))

(define/contract (olist-batches/c)
  (-> contract?)
  (fix/c olist-batches/c #/-> #/or/c
    (match/c olist-batch-rep-done olist<e0?)
    (match/c olist-batch-rep-stuck olist-batches/c)
    (match/c olist-batch-rep-plus 0<olist<e0?
      (match/c olist-batch-rep-stuck olist-batches/c))))

; Given a thunk which contains an ordinal numeral batch, where the
; `effect` of the batch is another such thunk, this returns an ordinal
; numeral computation that follows all those thunks.
;
; If the batches are actually infinite in number, it's recommended to
; have each batch increase in size to approach epsilon zero or another
; large-enough ordinal for your purposes. This is because if the
; maximum batch size doesn't exceed some bound N, then the act of
; calling `olist-codata-drop` with an amount of (N * omega) will cause
; the program to go into an infinite loop trying to compute all the
; batches.
;
; TODO: The same will be true for `olist-codata-untimes` and
; `olist-codata-log`, if and when we have those, at which point we'll
; want to encourage even stronger growth of batches as they go along.
; See the notes at `onum-batches->onum-codata`.
;
(define/contract (olist-batches->olist-codata batches)
  (-> (olist-batches/c) olist-codata?)
  (olist-codata batches))

(define/contract (olist->olist-codata n)
  (-> olist<=greatest-known? olist-codata?)
  (if (olist<e0? n) (olist-codata #/fn #/olist-batch-done n)
  ; NOTE: We allow ordinal-indexed lists of size epsilon zero to be
  ; converted to streams, but we don't allow them to be fully
  ; traversed. See the note at `onum->onum-codata`.
  #/onum-codata
    (w-loop next big-onum (onum-omega) n n
      (fn
        ; TODO: If we ever have `list-batch-times-onum` and
        ; `list-batch-pow-onum`, we should use them here. See the TODO
        ; on `onum-codata-unknowable`.
        (dissect (olist-drop big-onum n) (just #/list dropped n)
        #/olist-batch-plus-olist dropped
        #/onum-batch-stuck
        #/next (onum-pow (onum-omega) big-onum) n)))))

; NOTE: The arguments to `index->element` will all be ordinal numbers.
; As of right now, all of those ordinals will satisfy `onum<e0?`, but
; if and when future versions of this library add support for more
; ordinals, clients may find larger ordinals being passed to their
; `index->element` functions.
;
(define/contract (olist-codata-build len index->element)
  (-> onum-codata? (-> onum<=greatest-known? any/c) olist-codata?)
  (dissect len (onum-codata get-lenbatch)
  #/olist-codata
    (w-loop get-lenbatch->get-batch
      get-lenbatch get-lenbatch index->element index->element
      (fn
        (w- lenbatch (get-lenbatch)
        #/mat lenbatch (onum-batch-rep-done n)
          (olist-batch-done #/olist-build n index->element)
        #/mat lenbatch (onum-batch-rep-stuck effect)
          (#/get-lenbatch->get-batch effect index->element)
        #/dissect lenbatch (onum-batch-rep-plus a b)
          (olist-batch-rep-plus (olist-build a index->element)
            (get-lenbatch->get-batch b #/fn index
              (index->element #/onum-plus a index))))))))

(define/contract (olist-codata-length n)
  (-> olist-codata? onum-codata?)
  (dissect n (olist-codata get-batch)
  #/onum-codata
    (w-loop get-batch->get-lenbatch get-batch get-batch
      (fn
        (w- batch (get-batch)
        #/mat batch (olist-batch-rep-done n)
          (onum-batch-rep-done #/olist-length n)
        #/mat batch (olist-batch-rep-stuck effect)
          (#/get-batch->get-lenbatch effect)
        #/dissect batch (olist-batch-rep-plus a b)
          (onum-batch-rep-plus (olist-length a)
            (get-batch->get-lenbatch b)))))))

(define/contract (olist-codata-plus1 get-first n)
  (-> (-> any/c) olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/fn #/olist-batch-plus1 get-first #/get-batch))

(define/contract (olist-codata-plus-olist amount n)
  (-> olist<e0? olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/fn #/olist-batch-plus-olist amount #/get-batch))

(define/contract (olist-codata-drop1 n)
  (-> olist-codata? #/maybe/c #/list/c (-> any/c) olist-codata?)
  (dissect n (olist-codata get-batch)
  #/w- drop
    (w-loop drop []
      (olist-batch-drop1 #/fn b on-both-zero on-1-zero on-b-zero
        (dissect b (olist-batch-rep-stuck get-batch)
        #/drop (get-batch) on-both-zero on-1-zero on-b-zero)))
  #/drop (get-batch)
    (fn get-first #/just #/list get-first #/olist-zero)
    (fn get-first result #/just #/list get-first result)
    (fn #/nothing)))

(define/contract (olist-codata-drop amount n)
  (-> olist-codata? olist-codata?
    (maybe/c #/list/c olist<e0? olist-codata?))
  (dissect amount (olist-codata amount-get-batch)
  #/dissect n (olist-codata n-get-batch)
  #/w- drop
    (w-loop drop []
      (olist-codata-drop
      #/fn amount n on-both-zero on-amount-zero on-n-zero
        (w- unstick
          (fn n
            (mat n (olist-batch-rep-stuck n-get-batch)
              (n-get-batch)
              n))
        #/drop (unstick amount) (unstick n)
          on-both-zero on-amount-zero on-n-zero)))
  #/drop (amount-get-batch) (n-get-batch)
    (fn #/just #/list n (olist-zero))
    (fn n-dropped n-rest #/just #/list n-dropped n-rest)
    (fn amount-dropped amount-rest #/nothing)))

(define (olist-batches-fold state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (olist-batch-rep-done n)
      (dissect (func state n) (list state n)
      #/olist-batch-rep-done n)
    #/mat batch (olist-batch-rep-stuck effect)
      (olist-batches-fold state effect func)
    #/dissect batch (olist-batch-rep-plus a b)
      (dissect (func state a) (list state a)
      #/olist-batch-rep-plus a
        (olist-batches-fold state (fn b) func)))))

(define (olist-batches-fold-with-rest state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (olist-batch-rep-done n)
      (dissect (func state n #/olist->olist-codata #/olist-zero)
        (list state n)
      #/olist-batch-rep-done n)
    #/mat batch (olist-batch-rep-stuck effect)
      (olist-batches-fold-with-rest state effect func)
    #/dissect batch (olist-batch-rep-plus a b)
      (dissect (func state a #/olist-codata b) (list state a)
      #/olist-batch-rep-plus a
        (olist-batches-fold-with-rest state (fn b) func)))))

(define/contract (olist-codata-map n func)
  (-> olist-codata? (-> any/c any/c) olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/olist-batches-fold (trivial) get-batch #/fn state n
    (list state #/olist-map n func)))

(define/contract (olist-codata-map-kv n func)
  (-> olist-codata? (-> onum<=greatest-known? any/c any/c)
    olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/olist-batches-fold 0 get-batch #/fn state n
    (list (onum-plus state #/olist-length n)
      (olist-map-kv n #/fn i elem
        (func (onum-plus state i) elem)))))

(define/contract
  (olist-codata-zip-map a b on-a-rest on-b-rest on-elems)
  (->
    olist-codata?
    olist-codata?
    (-> olist-codata? olist-codata?)
    (-> olist-codata? olist-codata?)
    (-> any/c any/c any/c)
    olist-codata?)
  
  (define (extract-list get-batch)
    (w- batch (get-batch)
    #/mat batch (olist-batch-rep-done n)
      (list n #/fn #/olist-batch-done #/olist-zero)
    #/mat batch (olist-batch-rep-stuck effect)
      (list (olist-zero) effect)
    #/dissect batch (olist-batch-rep-plus a b)
      (list a #/fn b)))
  
  (dissect a (olist-codata get-a-batch)
  #/dissect b (olist-codata get-b-batch)
  #/olist-codata
    (w-loop next get-a-batch get-a-batch get-b-batch get-b-batch
      (fn
        (dissect (extract-list get-a-batch) (list a-list get-a-batch)
        #/dissect (extract-list get-b-batch) (list b-list get-b-batch)
        #/w- an (olist-length a-list)
        #/w- bn (olist-length b-list)
        #/w- process-inequal
          (fn
            small-n small-list get-small-batch
            big-n big-list get-big-batch
            on-big-rest
            on-elems-small-big
            
            (mat small-n 0
              ; If we're all out of one of the streams, then we call
              ; the appropriate `on-...-rest` callback to process the
              ; rest.
              (dissect
                (on-big-rest #/olist-codata #/fn
                  (olist-batch-plus-olist big-list get-big-batch))
                (olist-codata get-batch)
                get-batch)
            ; Otherwise, we can make at least some nonzero amount of
            ; progress by zipping as much of the current batches as we
            ; can.
            #/dissect (olist-drop small-n big-list)
              (just #/list big-list big-rest)
            #/olist-batch-plus-olist
              (olist-zip-map small-list big-list #/fn small big
                (on-elems-small-big small big))
              (olist-batch-stuck #/next get-small-batch #/fn
                (olist-batch-plus-olist big-rest #/get-big-batch))))
        #/w- comparison (onum-compare an bn)
        #/mat comparison '<
          (process-inequal
            an a-list get-a-batch bn b-list get-b-batch on-b-rest
            (fn a b #/on-elems a b))
        #/mat comparison '>
          (process-inequal
            bn b-list get-b-batch an a-list get-a-batch on-a-rest
            ; NOTE: We flip the arguments here, to make sure we're
            ; calling the original `on-elems` correctly.
            (fn b a #/on-elems a b))
        #/dissect comparison '=
          (olist-batch-plus-olist
            (olist-zip-map a-list b-list #/fn a b #/on-elems a b)
            (olist-batch-stuck #/fn
              (next get-a-batch get-b-batch))))))))

(define/contract (olist-codata-tails n)
  (-> olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/olist-batches-fold-with-rest (trivial) get-batch
  #/fn state n rest
    (list state
      (olist-map (olist-tails n) #/fn olist-tail
        (olist-codata-plus-olist olist-tail rest)))))

; Given an element thunk to put at position zero, a function to
; produce an element thunk at a successor position from the element
; thunk at its predecessor position, and an `on-limit` function to
; compute an element thunk at a limit position, this returns an
; ordinal-unbounded stream where every element is computed in those
; ways.
;
; The `on-limit` function must take an omega-sized list of element
; thunks from strictly ascending positions which approach the limit
; position. Its behavior *should* be the same regardless of which
; ascending sequence of positions is chosen, but this is not verified
; by the library.
;
(define/contract
  (olist-codata-transfinite-unfold-forever on-zero on-succ on-limit)
  (-> (-> any/c) (-> (-> any/c) #/-> any/c) (-> olist<e0? #/-> any/c)
    olist-codata?)
  
  ; NOTE: Because `onum-batch-drop` doesn't allow an `amount` of
  ; epsilon zero, this result is indistinguishable from a truly
  ; ordinal-unbounded result. See the note at `onum-batch-drop` for
  ; the reason it doesn't allow that.
  ;
  (olist->olist-codata #/olist-transfinite-unfold (onum-e0)
    on-zero on-succ on-limit))

(define/contract (olist-codata-ref-maybe-thunk n i)
  (-> olist-codata? onum<e0? #/maybe/c #/-> any/c)
  (maybe-bind
    (olist-codata-drop
      (olist->olist-codata #/olist-build i #/dissectfn _ #/trivial)
      n)
  #/dissectfn (list dropped n)
  #/maybe-bind (olist-codata-drop1 n) #/dissectfn (list get-first n)
  #/just get-first))

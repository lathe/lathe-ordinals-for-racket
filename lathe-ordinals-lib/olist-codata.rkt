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
  -> any/c contract? list/c or/c recursive-contract struct/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/maybe
  just maybe-bind maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in lathe-ordinals
  onum-compare onum-drop onum-e0 onum<=e0? onum<e0? 0<onum<e0?
  onum<=greatest-known? onum-plus)
(require #/only-in lathe-ordinals/olist
  olist-build olist-drop olist-drop1 olist<e0? olist<=e0? 0<olist<e0?
  olist-length olist-map olist-map-kv olist-plus olist-ref-thunk
  olist-tails olist-transfinite-unfold olist-zero olist-zip-map)

; TODO: Document all of these exports.
;
; TODO: Stop making almost all our structs mutable. We're only doing
; that for `struct/c`.
;
(provide

  onum-batch?
  onum->onum-batch
  part-and-rest->onum-batch
  onum-batch-unknowable
  onum-batch-plus
  onum-batch-plus1
  (struct-out onum-batch-drop-result-done)
  (struct-out onum-batch-drop-result-ready)
  (struct-out onum-batch-drop-result-stalled)
  onum-batch-drop-result?
  onum-batch-drop
  (struct-out onum-batch-drop1-result-done)
  (struct-out onum-batch-drop1-result-ready)
  (struct-out onum-batch-drop1-result-stalled)
  onum-batch-drop1-result?
  onum-batch-drop1

  olist-batch?
  olist->olist-batch
  part-and-rest->olist-batch
  olist-batch-plus
  olist-batch-plus1
  (struct-out olist-batch-drop-result-done)
  (struct-out olist-batch-drop-result-ready)
  (struct-out olist-batch-drop-result-stalled)
  olist-batch-drop-result?
  olist-batch-drop
  (struct-out olist-batch-drop1-result-done)
  (struct-out olist-batch-drop1-result-ready)
  (struct-out olist-batch-drop1-result-stalled)
  olist-batch-drop1-result?
  olist-batch-drop1
  
  (rename-out [-onum-codata? onum-codata?])
  onum-batches/c
  onum-batches->onum-codata
  onum->onum-codata
  onum-codata-unknowable
  onum-codata-plus1
  onum-codata-plus
  onum-codata-drop1
  onum-codata-drop
  
  (rename-out [-olist-codata? olist-codata?])
  olist-batches/c
  olist-batches->olist-codata
  olist->olist-codata olist-codata-build olist-codata-length
  olist-codata-plus1
  olist-codata-plus
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



(struct-easy (onum-batch-done onum) #:other #:mutable)
(struct-easy (onum-batch-progress onum rest) #:other #:mutable)

(define/contract (onum-batch? v)
  (-> any/c boolean?)
  (or (onum-batch-done? v) (onum-batch-progress? v)))

(define/contract (onum->onum-batch n)
  (-> onum<=e0? onum-batch?)
  (onum-batch-done n))

(define/contract (part-and-rest->onum-batch n rest)
  (-> 0<onum<e0? any/c onum-batch?)
  (onum-batch-progress n rest))

(define/contract (onum-batch-unknowable)
  (-> onum-batch?)
  
  ; NOTE: Because `onum-batch-drop` doesn't allow an `amount` of
  ; epsilon zero, this result is indistinguishable from a truly
  ; ordinal-unbounded result. See the note at `onum-batch-drop`
  ; for the reason it doesn't allow that.
  (onum->onum-batch #/onum-e0))

(define/contract (onum-batch-plus amount n)
  (-> onum<e0? onum-batch? onum-batch?)
  (mat n (onum-batch-done n)
    (onum-batch-done #/onum-plus amount n)
  #/dissect n (onum-batch-progress n rest)
    (onum-batch-progress (onum-plus amount n) rest)))

(define/contract (onum-batch-plus1 n)
  (-> onum-batch? onum-batch?)
  (onum-batch-plus 1 n))

(struct-easy (onum-batch-drop-result-done remainder)
  #:other #:mutable)
(struct-easy (onum-batch-drop-result-ready batch)
  #:other #:mutable)
(struct-easy (onum-batch-drop-result-stalled remainder rest)
  #:other #:mutable)

(define/contract (onum-batch-drop-result? v)
  (-> any/c boolean?)
  (or
    (onum-batch-drop-result-done? v)
    (onum-batch-drop-result-ready? v)
    (onum-batch-drop-result-stalled? v)))

; NOTE: We don't support an `amount` equal to epsilon zero here.
; Although our ordinal-indexed streams are not necessarily bounded by
; any ordinal (not necessarily even by an ordinal larger than epsilon
; zero), the amount a single program can ever take from the stream is
; still an ordinal. If a program takes from a single stream several
; times, we want the program to be able to store all those results in
; a single ordinal-indexed collection. So if it could take epsilon
; zero elements twice, that would mean we would want our
; ordinal-indexed collections to permit at least (2 * epsilon zero)
; elements, and we don't support that yet.
;
(define/contract (onum-batch-drop amount n)
  (-> onum<e0? onum-batch?
    (or/c
      (struct/c onum-batch-drop-result-done onum<e0?)
      (struct/c onum-batch-drop-result-ready onum-batch?)
      (struct/c onum-batch-drop-result-stalled onum<e0? any/c)))
  (mat n (onum-batch-done n)
    (mat (onum-drop amount n) (just n)
      (onum-batch-drop-result-ready #/onum-batch-done n)
    #/dissect (onum-drop n amount) (just remainder)
    #/onum-batch-drop-result-done remainder)
  #/dissect n (onum-batch-progress n rest)
  #/mat (onum-drop n amount) (just amount)
    (onum-batch-drop-result-stalled amount rest)
  #/dissect (onum-drop amount n) (just n)
  ; NOTE: At this point we know `n` is nonzero. If it were zero, we
  ; would have entered the `onum-batch-drop-result-stalled` case.
  #/onum-batch-drop-result-ready #/onum-batch-progress n rest))

(struct-easy (onum-batch-drop1-result-done) #:other #:mutable)
(struct-easy (onum-batch-drop1-result-ready batch) #:other #:mutable)
(struct-easy (onum-batch-drop1-result-stalled rest) #:other #:mutable)

(define/contract (onum-batch-drop1-result? v)
  (-> any/c boolean?)
  (or
    (onum-batch-drop1-result-done? v)
    (onum-batch-drop1-result-ready? v)
    (onum-batch-drop1-result-stalled? v)))

(define/contract (onum-batch-drop1 n)
  (-> onum-batch?
    (or/c
      (struct/c onum-batch-drop1-result-done)
      (struct/c onum-batch-drop1-result-ready onum-batch?)
      (struct/c onum-batch-drop1-result-stalled any/c)))
  (w- result (onum-batch-drop 1 n)
  #/mat result (onum-batch-drop-result-done 0)
    (onum-batch-drop1-result-done)
  #/mat result (onum-batch-drop-result-ready batch)
    (onum-batch-drop1-result-ready batch)
  #/dissect result (onum-batch-drop-result-stalled 0 rest)
    (onum-batch-drop1-result-stalled rest)))



(struct-easy (olist-batch-done olist)
  #:other #:mutable)
(struct-easy (olist-batch-progress olist rest)
  #:other #:mutable)

(define/contract (olist-batch? v)
  (-> any/c boolean?)
  (or (olist-batch-done? v) (olist-batch-progress? v)))

(define/contract (olist->olist-batch n)
  (-> olist<=e0? olist-batch?)
  (olist-batch-done n))

(define/contract (part-and-rest->olist-batch n rest)
  (-> 0<olist<e0? any/c olist-batch?)
  (olist-batch-progress n rest))

(define/contract (olist-batch-plus amount n)
  (-> olist<e0? olist-batch? olist-batch?)
  (mat n (olist-batch-done n)
    (olist-batch-done #/olist-plus amount n)
  #/dissect n (olist-batch-progress n rest)
    (olist-batch-progress (olist-plus amount n) rest)))

(define/contract (olist-batch-plus1 get-first n)
  (-> (-> any/c) olist-batch? olist-batch?)
  (olist-batch-plus (olist-build 1 #/dissectfn _ #/get-first) n))

(struct-easy (olist-batch-drop-result-done dropped remainder)
  #:other #:mutable)
(struct-easy (olist-batch-drop-result-ready dropped batch)
  #:other #:mutable)
(struct-easy (olist-batch-drop-result-stalled dropped remainder rest)
  #:other #:mutable)

(define/contract (olist-batch-drop-result? v)
  (-> any/c boolean?)
  (or
    (olist-batch-drop-result-done? v)
    (olist-batch-drop-result-ready? v)
    (olist-batch-drop-result-stalled? v)))

; NOTE: See the note at `onum-batch-drop` for the reason this doesn't
; accept an `amount` of epsilon zero.
(define/contract (olist-batch-drop amount n)
  (-> onum<e0? olist-batch?
    (or/c
      (struct/c olist-batch-drop-result-done olist<e0? onum<e0?)
      (struct/c olist-batch-drop-result-ready olist<e0? olist-batch?)
      (struct/c olist-batch-drop-result-stalled
        olist<e0? onum<e0? any/c)))
  (mat n (olist-batch-done n)
    (mat (olist-drop amount n) (just dropped-and-n)
      (dissect dropped-and-n (list dropped n)
      #/olist-batch-drop-result-ready dropped #/olist-batch-done n)
    #/dissect (onum-drop (olist-length n) amount) (just remainder)
    #/olist-batch-drop-result-done n remainder)
  #/dissect n (olist-batch-progress n rest)
  #/mat (onum-drop (olist-length n) amount) (just amount)
    (olist-batch-drop-result-stalled n amount rest)
  #/dissect (olist-drop amount n) (just #/list dropped n)
  ; NOTE: At this point we know `n` is nonempty. If it were empty, we
  ; would have entered the `olist-batch-drop-result-stalled` case.
  #/olist-batch-drop-result-ready
    dropped (olist-batch-progress n rest)))

(struct-easy (olist-batch-drop1-result-overdrawn)
  #:other #:mutable)
(struct-easy (olist-batch-drop1-result-done dropped)
  #:other #:mutable)
(struct-easy (olist-batch-drop1-result-ready dropped batch)
  #:other #:mutable)
(struct-easy (olist-batch-drop1-result-stalled dropped rest)
  #:other #:mutable)

(define/contract (olist-batch-drop1-result? v)
  (-> any/c boolean?)
  (or
    (olist-batch-drop1-result-overdrawn? v)
    (olist-batch-drop1-result-done? v)
    (olist-batch-drop1-result-ready? v)
    (olist-batch-drop1-result-stalled? v)))

(define/contract (olist-batch-drop1 n)
  (-> olist-batch?
    (or/c
      (struct/c olist-batch-drop1-result-overdrawn)
      (struct/c olist-batch-drop1-result-done (-> any/c))
      (struct/c olist-batch-drop1-result-ready (-> any/c)
        olist-batch?)
      (struct/c olist-batch-drop1-result-stalled (-> any/c) any/c)))
  (w- result (olist-batch-drop 1 n)
  #/mat result (olist-batch-drop-result-done dropped remainder)
    (mat remainder 1
      (olist-batch-drop1-result-overdrawn)
    #/olist-batch-drop1-result-done #/olist-ref-thunk dropped 0)
  #/mat result (olist-batch-drop-result-ready dropped batch)
    (olist-batch-drop1-result-ready (olist-ref-thunk dropped 0) batch)
  #/dissect result (olist-batch-drop-result-stalled dropped 0 rest)
    (olist-batch-drop1-result-stalled
      (olist-ref-thunk dropped 0)
      rest)))



(struct-easy (onum-codata get-batch))

; NOTE: This is just like `onum-codata?` except for its interaction
; with `struct-predicate-procedure?`.
(define/contract (-onum-codata? x)
  (-> any/c boolean?)
  (onum-codata? x))

(define/contract (onum-batches/c)
  (-> contract?)
  (->
    (or/c
      (struct/c onum-batch-done onum<=e0?)
      (struct/c onum-batch-progress 0<onum<e0?
        (recursive-contract #/onum-batches/c)))))

; Given a thunk which contains an ordinal numeral batch, where the
; "rest" of the batch is another such thunk, this returns an ordinal
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
(define/contract (onum-batches->onum-codata batches)
  (-> (onum-batches/c) onum-codata?)
  (onum-codata batches))

(define/contract (onum->onum-codata n)
  (-> onum<=e0? onum-codata?)
  (onum-codata #/fn #/onum->onum-batch n))

(define/contract (onum-codata-unknowable)
  (-> onum-codata?)
  (onum-codata #/onum-batch-unknowable))

(define/contract (onum-codata-plus1 n)
  (-> onum-codata? onum-codata?)
  (dissect n (onum-codata get-batch)
  #/onum-codata #/fn #/onum-batch-plus1 #/get-batch))

(define/contract (onum-codata-plus amount n)
  (-> onum<e0? onum-codata? onum-codata?)
  (dissect n (onum-codata get-batch)
  #/onum-codata #/fn #/onum-batch-plus amount #/get-batch))

(define/contract (onum-codata-drop1 n)
  (-> onum-codata? #/maybe/c onum-codata?)
  (dissect n (onum-codata get-batch)
  #/w- result (onum-batch-drop1 #/get-batch)
  #/mat result (onum-batch-drop1-result-done)
    (nothing)
  #/mat result (onum-batch-drop1-result-ready batch)
    (just #/onum-codata #/fn batch)
  #/dissect result (onum-batch-drop1-result-stalled rest)
    (just #/onum-codata rest)))

; NOTE: See the note at `onum-batch-drop` for the reason this doesn't
; accept an `amount` of epsilon zero.
(define/contract (onum-codata-drop amount n)
  (-> onum<e0? onum-codata? #/maybe/c onum-codata?)
  (dissect n (onum-codata get-batch)
  #/w- result (onum-batch-drop amount #/get-batch)
  #/mat result (onum-batch-drop-result-done remainder)
    (expect remainder 0 (nothing)
    #/just #/onum-codata #/fn #/onum->onum-batch 0)
  #/mat result (onum-batch-drop-result-ready batch)
    (just #/onum-codata #/fn batch)
  #/dissect result (onum-batch-drop-result-stalled remainder rest)
  #/mat remainder 0
    (just #/onum-codata rest)
  #/onum-codata-drop remainder #/onum-codata rest))



(struct-easy (olist-codata get-batch))

; NOTE: This is just like `olist-codata?` except for its interaction
; with `struct-predicate-procedure?`.
(define/contract (-olist-codata? x)
  (-> any/c boolean?)
  (olist-codata? x))

(define/contract (olist-batches/c)
  (-> contract?)
  (->
    (or/c
      (struct/c olist-batch-done olist<=e0?)
      (struct/c olist-batch-progress 0<olist<e0?
        (recursive-contract #/olist-batches/c)))))

; Given a thunk which contains an ordinal-indexed stream batch, where
; the "rest" of the batch is another such thunk, this returns an
; ordinal-indexed stream that follows all those thunks.
;
; If the batches are actually infinite in number, it's recommended to
; have each batch increase in size to approach epsilon zero or another
; large-enough ordinal for your purposes. This is because if the
; maximum batch size doesn't exceed some bound N, then the act of
; calling `olist-codata-drop` with an amount of (N * omega) will cause
; the program to go into an infinite loop trying to compute all the
; batches.
;
(define/contract (olist-batches->olist-codata batches)
  (-> (olist-batches/c) olist-codata?)
  (olist-codata batches))

(define/contract (olist->olist-codata n)
  (-> olist<=e0? olist-codata?)
  (olist-codata #/fn #/olist->olist-batch n))

; NOTE: The arguments to `index->element` will all be ordinal numbers.
; As of right now, all of those ordinals will satisfy `onum<=e0?`, but
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
        #/mat lenbatch (onum-batch-done n)
          (olist-batch-done #/olist-build n index->element)
        #/dissect lenbatch (onum-batch-progress n rest)
          (olist-batch-progress (olist-build n index->element)
            (get-lenbatch->get-batch rest #/fn index
              (index->element #/onum-plus n index))))))))

(define/contract (olist-codata-length n)
  (-> olist-codata? onum-codata?)
  (dissect n (olist-codata get-batch)
  #/onum-codata
    (w-loop get-batch->get-lenbatch get-batch get-batch
      (fn
        (w- batch (get-batch)
        #/mat batch (olist-batch-done n)
          (onum-batch-done #/olist-length n)
        #/dissect batch (olist-batch-progress n rest)
          (onum-batch-progress (olist-length n)
            (get-batch->get-lenbatch rest)))))))

(define/contract (olist-codata-plus1 get-first n)
  (-> (-> any/c) olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/fn #/olist-batch-plus1 get-first #/get-batch))

(define/contract (olist-codata-plus amount n)
  (-> olist<e0? olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/fn #/olist-batch-plus amount #/get-batch))

(define/contract (olist-codata-drop1 n)
  (-> olist-codata? #/maybe/c #/list/c (-> any/c) olist-codata?)
  (dissect n (olist-codata get-batch)
  #/dissect (olist-batch-drop1 #/get-batch) (list dropped result)
  #/mat result (olist-batch-drop1-result-overdrawn)
    (nothing)
  #/mat result (olist-batch-drop1-result-done get-first)
    (just #/list get-first
      (olist-codata #/fn #/olist->olist-batch #/olist-zero))
  #/mat result (olist-batch-drop1-result-ready get-first batch)
    (just #/list get-first #/olist-codata #/fn batch)
  #/dissect result (olist-batch-drop1-result-stalled get-first rest)
    (just #/list get-first #/olist-codata rest)))

; NOTE: See the note at `onum-batch-drop` for the reason this doesn't
; accept an `amount` of epsilon zero.
(define/contract (olist-codata-drop amount n)
  (-> onum<e0? olist-codata?
    (maybe/c #/list/c olist<e0? olist-codata?))
  (w-loop next dropped-so-far (olist-zero) amount amount n n
    (dissect n (olist-codata get-batch)
    #/w- result (olist-batch-drop amount #/get-batch)
    #/mat result (olist-batch-drop-result-done dropped remainder)
      (expect remainder 0 (nothing)
      #/just #/list (olist-plus dropped-so-far dropped)
        (olist-codata #/fn #/olist->olist-batch #/olist-zero))
    #/mat result (olist-batch-drop-result-ready dropped batch)
      (just #/list (olist-plus dropped-so-far dropped)
        (olist-codata #/fn batch))
    #/dissect result
      (olist-batch-drop-result-stalled dropped remainder rest)
      (mat remainder 0
        (just (olist-plus dropped-so-far dropped) #/olist-codata rest)
        (next
          ; TODO: See if the way we're concatenating the
          ; `dropped-so-far` entries is a painter's algorithm. What we
          ; should probably do is to make `olist-rep-plus` use a
          ; catenable deque data structure for all the summands.
          (olist-plus dropped-so-far dropped)
          remainder
          (olist-codata rest))))))

(define (onum-batches-fold state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (olist-batch-done n)
      (dissect (func state n) (list state n)
      #/olist-batch-done n)
    #/dissect batch (olist-batch-progress n rest)
      (dissect (func state n) (list state n)
      #/olist-batch-progress n
        (onum-batches-fold state rest func)))))

(define (onum-batches-fold-with-rest state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (olist-batch-done n)
      (dissect (func state n #/olist->olist-codata #/olist-zero)
        (list state n)
      #/olist-batch-done n)
    #/dissect batch (olist-batch-progress n rest)
      (dissect (func state n #/olist-codata rest) (list state n)
      #/olist-batch-progress n
        (onum-batches-fold state rest func)))))

(define/contract (olist-codata-map n func)
  (-> olist-codata? (-> any/c any/c) olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/onum-batches-fold (trivial) get-batch #/fn state n
    (list state #/olist-map n func)))

(define/contract (olist-codata-map-kv n func)
  (-> olist-codata? (-> onum<=greatest-known? any/c any/c)
    olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/onum-batches-fold 0 get-batch #/fn state n
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
    #/mat batch (olist-batch-done n)
      (list n #/fn #/olist-batch-done #/olist-zero)
    #/dissect batch (olist-batch-progress n rest)
      (list n rest)))
  
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
                  (olist-batch-plus big-list get-big-batch))
                (olist-codata get-batch)
                get-batch)
            ; Otherwise, we can make at least some nonzero amount of
            ; progress by zipping as much of the current batches as we
            ; can.
            #/dissect (olist-drop small-n big-list)
              (just #/list big-list big-rest)
            #/olist-batch-progress
              (olist-zip-map small-list big-list #/fn small big
                (on-elems-small-big small big))
              (next
                get-small-batch
                (fn #/olist-batch-plus big-rest #/get-big-batch))))
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
          (olist-batch-progress
            (olist-zip-map a-list b-list #/fn a b #/on-elems a b)
            (next get-a-batch get-b-batch)))))))

(define/contract (olist-codata-tails n)
  (-> olist-codata? olist-codata?)
  (dissect n (olist-codata get-batch)
  #/olist-codata #/onum-batches-fold-with-rest (trivial) get-batch
  #/fn state n rest
    (list state
      (olist-map (olist-tails n) #/fn olist-tail
        (olist-codata-plus olist-tail rest)))))

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
  (-> olist-codata? onum<=e0? #/maybe/c #/-> any/c)
  (maybe-bind (olist-codata-drop i n) #/dissectfn (list dropped n)
  #/maybe-bind (olist-codata-drop1 n) #/dissectfn (list get-first n)
  #/just get-first))

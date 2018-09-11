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
; "ordinal-indexed stream lengths" since they're isomorphic to
; ordinal-indexed streams with trivial elements.
;
; Our representation uses ordinal-sized "batches" so that we can read
; infinite-ordinal-sized chunks of data from a stream in a program
; that still terminates. We provide these batches -- essentially
; ordinal-indexed *improper lists* -- as data structures of their own.
; They could be useful for implementing other stream transformations.

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

  ostreamlenbatch?
  onum->ostreamlenbatch
  part-and-rest->ostreamlenbatch
  ostreamlenbatch-unknowable
  ostreamlenbatch-plus
  ostreamlenbatch-plus1
  (struct-out ostreamlenbatch-drop-result-done)
  (struct-out ostreamlenbatch-drop-result-ready)
  (struct-out ostreamlenbatch-drop-result-stalled)
  ostreamlenbatch-drop-result?
  ostreamlenbatch-drop
  (struct-out ostreamlenbatch-drop1-result-done)
  (struct-out ostreamlenbatch-drop1-result-ready)
  (struct-out ostreamlenbatch-drop1-result-stalled)
  ostreamlenbatch-drop1-result?
  ostreamlenbatch-drop1

  ostreambatch?
  olist->ostreambatch
  part-and-rest->ostreambatch
  ostreambatch-plus
  ostreambatch-plus1
  (struct-out ostreambatch-drop-result-done)
  (struct-out ostreambatch-drop-result-ready)
  (struct-out ostreambatch-drop-result-stalled)
  ostreambatch-drop-result?
  ostreambatch-drop
  (struct-out ostreambatch-drop1-result-done)
  (struct-out ostreambatch-drop1-result-ready)
  (struct-out ostreambatch-drop1-result-stalled)
  ostreambatch-drop1-result?
  ostreambatch-drop1
  
  (rename-out [-ostreamlen? ostreamlen?])
  ostreamlenbatches/c
  ostreamlenbatches->ostreamlen
  onum->ostreamlen
  ostreamlen-unknowable
  ostreamlen-plus1
  ostreamlen-plus
  ostreamlen-drop1
  ostreamlen-drop
  
  (rename-out [-ostream? ostream?])
  ostreambatches/c
  ostreambatches->ostream
  olist->ostream ostream-build ostream-length
  ostream-plus1
  ostream-plus
  ostream-drop1
  ostream-drop
  
  ; TODO: See if we can define operations analogous to these, for each
  ; of `ostreamlenbatch...`, `ostreambatch...`, `ostreamlen...`, and
  ; `ostream...`.
  ;
;  onum-times-list onum-times
;  onum-untimes
;  onum->limit-plus-finite
;  onum-pred-maybe
;  onum-pow-list onum-pow
  ;
  ; TODO: If we ever implement an `onum-log`, consider its analogous
  ; operations here as as well.
  
  ; TODO: See if we'll ever use `ostream-map-kv` -- or any of the
  ; others, for that matter.
  ostream-map ostream-map-kv ostream-zip-map
  ostream-tails
  ostream-transfinite-unfold-forever
  
  ostream-ref-maybe-thunk
)



(struct-easy (ostreamlenbatch-done onum)
  #:other #:mutable)
(struct-easy (ostreamlenbatch-progress onum rest)
  #:other #:mutable)

(define/contract (ostreamlenbatch? v)
  (-> any/c boolean?)
  (or (ostreamlenbatch-done? v) (ostreamlenbatch-progress? v)))

(define/contract (onum->ostreamlenbatch n)
  (-> onum<=e0? ostreamlenbatch?)
  (ostreamlenbatch-done n))

(define/contract (part-and-rest->ostreamlenbatch n rest)
  (-> 0<onum<e0? any/c ostreamlenbatch?)
  (ostreamlenbatch-progress n rest))

(define/contract (ostreamlenbatch-unknowable)
  (-> ostreamlenbatch?)
  
  ; NOTE: Because `ostreamlenbatch-drop` doesn't allow an `amount` of
  ; epsilon zero, this result is indistinguishable from a truly
  ; ordinal-unbounded result. See the note at `ostreamlenbatch-drop`
  ; for the reason it doesn't allow that.
  (onum->ostreamlenbatch #/onum-e0))

(define/contract (ostreamlenbatch-plus amount n)
  (-> onum<e0? ostreamlenbatch? ostreamlenbatch?)
  (mat n (ostreamlenbatch-done n)
    (ostreamlenbatch-done #/onum-plus amount n)
  #/dissect n (ostreamlenbatch-progress n rest)
    (ostreamlenbatch-progress (onum-plus amount n) rest)))

(define/contract (ostreamlenbatch-plus1 n)
  (-> ostreamlenbatch? ostreamlenbatch?)
  (ostreamlenbatch-plus 1 n))

(struct-easy (ostreamlenbatch-drop-result-done remainder)
  #:other #:mutable)
(struct-easy (ostreamlenbatch-drop-result-ready batch)
  #:other #:mutable)
(struct-easy (ostreamlenbatch-drop-result-stalled remainder rest)
  #:other #:mutable)

(define/contract (ostreamlenbatch-drop-result? v)
  (-> any/c boolean?)
  (or
    (ostreamlenbatch-drop-result-done? v)
    (ostreamlenbatch-drop-result-ready? v)
    (ostreamlenbatch-drop-result-stalled? v)))

; NOTE: We don't support an `amount` equal to epsilon zero here.
; Although our ordinal-sized streams are not necessarily by any
; ordinal (not necessarily even by an ordinal larger than epsilon
; zero), the amount a single program can ever take from the stream is
; still an ordinal. If a program takes from a single stream several
; times, we want the program to be able to store all those results in
; a single ordinal-bounded collection. So if it could take epsilon
; zero elements twice, that would mean we would want our
; ordinal-bounded collections to permit at least (2 * epsilon zero)
; elements, and we don't support that yet.
;
(define/contract (ostreamlenbatch-drop amount n)
  (-> onum<e0? ostreamlenbatch?
    (or/c
      (struct/c ostreamlenbatch-drop-result-done onum<e0?)
      (struct/c ostreamlenbatch-drop-result-ready ostreamlenbatch?)
      (struct/c ostreamlenbatch-drop-result-stalled onum<e0? any/c)))
  (mat n (ostreamlenbatch-done n)
    (mat (onum-drop amount n) (just n)
      (ostreamlenbatch-drop-result-ready #/ostreamlenbatch-done n)
    #/dissect (onum-drop n amount) (just remainder)
    #/ostreamlenbatch-drop-result-done remainder)
  #/dissect n (ostreamlenbatch-progress n rest)
  #/mat (onum-drop n amount) (just amount)
    (ostreamlenbatch-drop-result-stalled amount rest)
  #/dissect (onum-drop amount n) (just n)
  ; NOTE: At this point we know `n` is nonzero. If it were zero, we
  ; would have entered the `ostreamlenbatch-drop-result-stalled` case.
  #/ostreamlenbatch-drop-result-ready #/ostreamlenbatch-progress
    n rest))

(struct-easy (ostreamlenbatch-drop1-result-done)
  #:other #:mutable)
(struct-easy (ostreamlenbatch-drop1-result-ready batch)
  #:other #:mutable)
(struct-easy (ostreamlenbatch-drop1-result-stalled rest)
  #:other #:mutable)

(define/contract (ostreamlenbatch-drop1-result? v)
  (-> any/c boolean?)
  (or
    (ostreamlenbatch-drop1-result-done? v)
    (ostreamlenbatch-drop1-result-ready? v)
    (ostreamlenbatch-drop1-result-stalled? v)))

(define/contract (ostreamlenbatch-drop1 n)
  (-> ostreamlenbatch?
    (or/c
      (struct/c ostreamlenbatch-drop1-result-done)
      (struct/c ostreamlenbatch-drop1-result-ready ostreamlenbatch?)
      (struct/c ostreamlenbatch-drop1-result-stalled any/c)))
  (w- result (ostreamlenbatch-drop 1 n)
  #/mat result (ostreamlenbatch-drop-result-done 0)
    (ostreamlenbatch-drop1-result-done)
  #/mat result (ostreamlenbatch-drop-result-ready batch)
    (ostreamlenbatch-drop1-result-ready batch)
  #/dissect result (ostreamlenbatch-drop-result-stalled 0 rest)
    (ostreamlenbatch-drop1-result-stalled rest)))



(struct-easy (ostreambatch-done olist)
  #:other #:mutable)
(struct-easy (ostreambatch-progress olist rest)
  #:other #:mutable)

(define/contract (ostreambatch? v)
  (-> any/c boolean?)
  (or (ostreambatch-done? v) (ostreambatch-progress? v)))

(define/contract (olist->ostreambatch n)
  (-> olist<=e0? ostreambatch?)
  (ostreambatch-done n))

(define/contract (part-and-rest->ostreambatch n rest)
  (-> 0<olist<e0? any/c ostreambatch?)
  (ostreambatch-progress n rest))

(define/contract (ostreambatch-plus amount n)
  (-> olist<e0? ostreambatch? ostreambatch?)
  (mat n (ostreambatch-done n)
    (ostreambatch-done #/olist-plus amount n)
  #/dissect n (ostreambatch-progress n rest)
    (ostreambatch-progress (olist-plus amount n) rest)))

(define/contract (ostreambatch-plus1 get-first n)
  (-> (-> any/c) ostreambatch? ostreambatch?)
  (ostreambatch-plus (olist-build 1 #/dissectfn _ #/get-first) n))

(struct-easy (ostreambatch-drop-result-done dropped remainder)
  #:other #:mutable)
(struct-easy (ostreambatch-drop-result-ready dropped batch)
  #:other #:mutable)
(struct-easy
  (ostreambatch-drop-result-stalled dropped remainder rest)
  #:other #:mutable)

(define/contract (ostreambatch-drop-result? v)
  (-> any/c boolean?)
  (or
    (ostreambatch-drop-result-done? v)
    (ostreambatch-drop-result-ready? v)
    (ostreambatch-drop-result-stalled? v)))

; NOTE: See the note at `ostreamlenbatch-drop` for the reason this
; doesn't accept an `amount` of epsilon zero.
(define/contract (ostreambatch-drop amount n)
  (-> onum<e0? ostreambatch?
    (or/c
      (struct/c ostreambatch-drop-result-done olist<e0? onum<e0?)
      (struct/c ostreambatch-drop-result-ready
        olist<e0? ostreambatch?)
      (struct/c ostreambatch-drop-result-stalled
        olist<e0? onum<e0? any/c)))
  (mat n (ostreambatch-done n)
    (mat (olist-drop amount n) (just dropped-and-n)
      (dissect dropped-and-n (list dropped n)
      #/ostreambatch-drop-result-ready dropped #/ostreambatch-done n)
    #/dissect (onum-drop (olist-length n) amount) (just remainder)
    #/ostreambatch-drop-result-done n remainder)
  #/dissect n (ostreambatch-progress n rest)
  #/mat (onum-drop (olist-length n) amount) (just amount)
    (ostreambatch-drop-result-stalled n amount rest)
  #/dissect (olist-drop amount n) (just #/list dropped n)
  ; NOTE: At this point we know `n` is nonempty. If it were empty, we
  ; would have entered the `ostreambatch-drop-result-stalled` case.
  #/ostreambatch-drop-result-ready
    dropped (ostreambatch-progress n rest)))

(struct-easy (ostreambatch-drop1-result-overdrawn)
  #:other #:mutable)
(struct-easy (ostreambatch-drop1-result-done dropped)
  #:other #:mutable)
(struct-easy (ostreambatch-drop1-result-ready dropped batch)
  #:other #:mutable)
(struct-easy (ostreambatch-drop1-result-stalled dropped rest)
  #:other #:mutable)

(define/contract (ostreambatch-drop1-result? v)
  (-> any/c boolean?)
  (or
    (ostreambatch-drop1-result-overdrawn? v)
    (ostreambatch-drop1-result-done? v)
    (ostreambatch-drop1-result-ready? v)
    (ostreambatch-drop1-result-stalled? v)))

(define/contract (ostreambatch-drop1 n)
  (-> ostreambatch?
    (or/c
      (struct/c ostreambatch-drop1-result-overdrawn)
      (struct/c ostreambatch-drop1-result-done (-> any/c))
      (struct/c ostreambatch-drop1-result-ready (-> any/c)
        ostreambatch?)
      (struct/c ostreambatch-drop1-result-stalled (-> any/c) any/c)))
  (w- result (ostreambatch-drop 1 n)
  #/mat result (ostreambatch-drop-result-done dropped remainder)
    (mat remainder 1
      (ostreambatch-drop1-result-overdrawn)
    #/ostreambatch-drop1-result-done #/olist-ref-thunk dropped 0)
  #/mat result (ostreambatch-drop-result-ready dropped batch)
    (ostreambatch-drop1-result-ready
      (olist-ref-thunk dropped 0)
      batch)
  #/dissect result (ostreambatch-drop-result-stalled dropped 0 rest)
    (ostreambatch-drop1-result-stalled
      (olist-ref-thunk dropped 0)
      rest)))



(struct-easy (ostreamlen get-batch))

; NOTE: This is just like `ostreamlen?` except for its interaction
; with `struct-predicate-procedure?`.
(define/contract (-ostreamlen? x)
  (-> any/c boolean?)
  (ostreamlen? x))

(define/contract (ostreamlenbatches/c)
  (-> contract?)
  (->
    (or/c
      (struct/c ostreamlenbatch-done onum<=e0?)
      (struct/c ostreamlenbatch-progress 0<onum<e0?
        (recursive-contract #/ostreamlenbatches/c)))))

; Given a thunk which contains an ordinal-indexed stream length batch,
; where the "rest" of the batch is another such thunk, this returns an
; ordinal-indexed stream length that represents the result of summing
; all those batches.
;
; If the batches are actually infinite in number, it's recommended to
; have each batch increase in size to approach epsilon zero or another
; large-enough ordinal for your purposes. This is because if the
; stream's maximum batch size doesn't exceed some bound N, then the
; act of calling `ostreamlen-drop` with an amount of (N * omega) will
; cause the program to go into an infinite loop trying to collect all
; the batches.
;
(define/contract (ostreamlenbatches->ostreamlen batches)
  (-> (ostreamlenbatches/c) ostreamlen?)
  (ostreamlen batches))

(define/contract (onum->ostreamlen n)
  (-> onum<=e0? ostreamlen?)
  (ostreamlen #/fn #/onum->ostreamlenbatch n))

(define/contract (ostreamlen-unknowable)
  (-> ostreamlen?)
  (ostreamlen #/ostreamlenbatch-unknowable))

(define/contract (ostreamlen-plus1 n)
  (-> ostreamlen? ostreamlen?)
  (dissect n (ostreamlen get-batch)
  #/ostreamlen #/fn #/ostreamlenbatch-plus1 #/get-batch))

(define/contract (ostreamlen-plus amount n)
  (-> onum<e0? ostreamlen? ostreamlen?)
  (dissect n (ostreamlen get-batch)
  #/ostreamlen #/fn #/ostreamlenbatch-plus amount #/get-batch))

(define/contract (ostreamlen-drop1 n)
  (-> ostreamlen? #/maybe/c ostreamlen?)
  (dissect n (ostreamlen get-batch)
  #/w- result (ostreamlenbatch-drop1 #/get-batch)
  #/mat result (ostreamlenbatch-drop1-result-done)
    (nothing)
  #/mat result (ostreamlenbatch-drop1-result-ready batch)
    (just #/ostreamlen #/fn batch)
  #/dissect result (ostreamlenbatch-drop1-result-stalled rest)
    (just #/ostreamlen rest)))

; NOTE: See the note at `ostreamlenbatch-drop` for the reason this
; doesn't accept an `amount` of epsilon zero.
(define/contract (ostreamlen-drop amount n)
  (-> onum<e0? ostreamlen? #/maybe/c ostreamlen?)
  (dissect n (ostreamlen get-batch)
  #/w- result (ostreamlenbatch-drop amount #/get-batch)
  #/mat result (ostreamlenbatch-drop-result-done remainder)
    (expect remainder 0 (nothing)
    #/just #/ostreamlen #/fn #/onum->ostreamlenbatch 0)
  #/mat result (ostreamlenbatch-drop-result-ready batch)
    (just #/ostreamlen #/fn batch)
  #/dissect result
    (ostreamlenbatch-drop-result-stalled remainder rest)
  #/mat remainder 0
    (just #/ostreamlen rest)
  #/ostreamlen-drop remainder #/ostreamlen rest))



(struct-easy (ostream get-batch))

; NOTE: This is just like `ostream?` except for its interaction with
; `struct-predicate-procedure?`.
(define/contract (-ostream? x)
  (-> any/c boolean?)
  (ostream? x))

(define/contract (ostreambatches/c)
  (-> contract?)
  (->
    (or/c
      (struct/c ostreambatch-done olist<=e0?)
      (struct/c ostreambatch-progress 0<olist<e0?
        (recursive-contract #/ostreambatches/c)))))

; Given a thunk which contains an ordinal-indexed stream batch, where
; the "rest" of the batch is another such thunk, this returns an
; ordinal-indexed stream that represents the result of composing all
; those batches.
;
; If the batches are actually infinite in number, it's recommended to
; have each batch increase in size to approach epsilon zero or another
; large-enough ordinal for your purposes. This is because if the
; stream's maximum batch size doesn't exceed some bound N, then the
; act of calling `ostream-drop` with an amount of (N * omega) will
; cause the program to go into an infinite loop trying to collect all
; the batches.
;
(define/contract (ostreambatches->ostream batches)
  (-> (ostreambatches/c) ostream?)
  (ostream batches))

(define/contract (olist->ostream n)
  (-> olist<=e0? ostream?)
  (ostream #/fn #/olist->ostreambatch n))

; NOTE: The arguments to `index->element` will all be ordinal numbers.
; As of right now, all of those ordinals will satisfy `onum<=e0?`, but
; if and when future versions of this library add support for more
; ordinals, clients may find larger ordinals being passed to their
; `index->element` functions.
;
(define/contract (ostream-build len index->element)
  (-> ostreamlen? (-> onum<=greatest-known? any/c) ostream?)
  (dissect len (ostreamlen get-lenbatch)
  #/ostream
    (w-loop get-lenbatch->get-batch
      get-lenbatch get-lenbatch index->element index->element
      (fn
        (w- lenbatch (get-lenbatch)
        #/mat lenbatch (ostreamlenbatch-done n)
          (ostreambatch-done #/olist-build n index->element)
        #/dissect lenbatch (ostreamlenbatch-progress n rest)
          (ostreambatch-progress (olist-build n index->element)
            (get-lenbatch->get-batch rest #/fn index
              (index->element #/onum-plus n index))))))))

(define/contract (ostream-length n)
  (-> ostream? ostreamlen?)
  (dissect n (ostream get-batch)
  #/ostreamlen
    (w-loop get-batch->get-lenbatch get-batch get-batch
      (fn
        (w- batch (get-batch)
        #/mat batch (ostreambatch-done n)
          (ostreamlenbatch-done #/olist-length n)
        #/dissect batch (ostreambatch-progress n rest)
          (ostreamlenbatch-progress (olist-length n)
            (get-batch->get-lenbatch rest)))))))

(define/contract (ostream-plus1 get-first n)
  (-> (-> any/c) ostream? ostream?)
  (dissect n (ostream get-batch)
  #/ostream #/fn #/ostreambatch-plus1 get-first #/get-batch))

(define/contract (ostream-plus amount n)
  (-> olist<e0? ostream? ostream?)
  (dissect n (ostream get-batch)
  #/ostream #/fn #/ostreambatch-plus amount #/get-batch))

(define/contract (ostream-drop1 n)
  (-> ostream? #/maybe/c #/list/c (-> any/c) ostream?)
  (dissect n (ostream get-batch)
  #/dissect (ostreambatch-drop1 #/get-batch) (list dropped result)
  #/mat result (ostreambatch-drop1-result-overdrawn)
    (nothing)
  #/mat result (ostreambatch-drop1-result-done get-first)
    (just #/list get-first
      (ostream #/fn #/olist->ostreambatch #/olist-zero))
  #/mat result (ostreambatch-drop1-result-ready get-first batch)
    (just #/list get-first #/ostream #/fn batch)
  #/dissect result (ostreambatch-drop1-result-stalled get-first rest)
    (just #/list get-first #/ostream rest)))

; NOTE: See the note at `ostreamlenbatch-drop` for the reason this
; doesn't accept an `amount` of epsilon zero.
(define/contract (ostream-drop amount n)
  (-> onum<e0? ostream? #/maybe/c #/list/c olist<e0? ostream?)
  (w-loop next dropped-so-far (olist-zero) amount amount n n
    (dissect n (ostream get-batch)
    #/w- result (ostreambatch-drop amount #/get-batch)
    #/mat result (ostreambatch-drop-result-done dropped remainder)
      (expect remainder 0 (nothing)
      #/just #/list (olist-plus dropped-so-far dropped)
        (ostream #/fn #/olist->ostreambatch #/olist-zero))
    #/mat result (ostreambatch-drop-result-ready dropped batch)
      (just #/list (olist-plus dropped-so-far dropped)
        (ostream #/fn batch))
    #/dissect result
      (ostreambatch-drop-result-stalled dropped remainder rest)
      (mat remainder 0
        (just (olist-plus dropped-so-far dropped) #/ostream rest)
        (next
          ; TODO: See if the way we're concatenating the
          ; `dropped-so-far` entries is a painter's algorithm. What we
          ; should probably do is to make `olist-rep-plus` use a
          ; catenable deque data structure for all the summands.
          (olist-plus dropped-so-far dropped)
          remainder
          (ostream rest))))))

(define (ostreamlenbatches-fold state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (ostreambatch-done n)
      (dissect (func state n) (list state n)
      #/ostreambatch-done n)
    #/dissect batch (ostreambatch-progress n rest)
      (dissect (func state n) (list state n)
      #/ostreambatch-progress n
        (ostreamlenbatches-fold state rest func)))))

(define (ostreamlenbatches-fold-with-rest state get-batch func)
  (fn
    (w- batch (get-batch)
    #/mat batch (ostreambatch-done n)
      (dissect (func state n #/olist->ostream #/olist-zero)
        (list state n)
      #/ostreambatch-done n)
    #/dissect batch (ostreambatch-progress n rest)
      (dissect (func state n #/ostream rest) (list state n)
      #/ostreambatch-progress n
        (ostreamlenbatches-fold state rest func)))))

(define/contract (ostream-map n func)
  (-> ostream? (-> any/c any/c) ostream?)
  (dissect n (ostream get-batch)
  #/ostream #/ostreamlenbatches-fold (trivial) get-batch #/fn state n
    (list state #/olist-map n func)))

(define/contract (ostream-map-kv n func)
  (-> ostream? (-> onum<=greatest-known? any/c any/c) ostream?)
  (dissect n (ostream get-batch)
  #/ostream #/ostreamlenbatches-fold 0 get-batch #/fn state n
    (list (onum-plus state #/olist-length n)
      (olist-map-kv n #/fn i elem
        (func (onum-plus state i) elem)))))

(define/contract (ostream-zip-map a b on-a-rest on-b-rest on-elems)
  (->
    ostream?
    ostream?
    (-> ostream? ostream?)
    (-> ostream? ostream?)
    (-> any/c any/c any/c)
    ostream?)
  
  (define (extract-list get-batch)
    (w- batch (get-batch)
    #/mat batch (ostreambatch-done n)
      (list n #/fn #/ostreambatch-done #/olist-zero)
    #/dissect batch (ostreambatch-progress n rest)
      (list n rest)))
  
  (dissect a (ostream get-a-batch)
  #/dissect b (ostream get-b-batch)
  #/ostream
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
                (on-big-rest #/ostream #/fn
                  (ostreambatch-plus big-list get-big-batch))
                (ostream get-batch)
                get-batch)
            ; Otherwise, we can make at least some nonzero amount of
            ; progress by zipping as much of the current batches as we
            ; can.
            #/dissect (olist-drop small-n big-list)
              (just #/list big-list big-rest)
            #/ostreambatch-progress
              (olist-zip-map small-list big-list #/fn small big
                (on-elems-small-big small big))
              (next
                get-small-batch
                (fn #/ostreambatch-plus big-rest #/get-big-batch))))
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
          (ostreambatch-progress
            (olist-zip-map a-list b-list #/fn a b #/on-elems a b)
            (next get-a-batch get-b-batch)))))))

(define/contract (ostream-tails n)
  (-> ostream? ostream?)
  (dissect n (ostream get-batch)
  #/ostream #/ostreamlenbatches-fold-with-rest (trivial) get-batch
  #/fn state n rest
    (list state
      (olist-map (olist-tails n) #/fn olist-tail
        (ostream-plus olist-tail rest)))))

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
  (ostream-transfinite-unfold-forever on-zero on-succ on-limit)
  (-> (-> any/c) (-> (-> any/c) #/-> any/c) (-> olist<e0? #/-> any/c)
    ostream?)
  
  ; NOTE: Because `ostreamlenbatch-drop` doesn't allow an `amount` of
  ; epsilon zero, this result is indistinguishable from a truly
  ; ordinal-unbounded result. See the note at `ostreamlenbatch-drop`
  ; for the reason it doesn't allow that.
  ;
  (olist->ostream #/olist-transfinite-unfold (onum-e0)
    on-zero on-succ on-limit))

(define/contract (ostream-ref-maybe-thunk n i)
  (-> ostream? onum<=e0? #/maybe/c #/-> any/c)
  (maybe-bind (ostream-drop i n) #/dissectfn (list dropped n)
  #/maybe-bind (ostream-drop1 n) #/dissectfn (list get-first n)
  #/just get-first))

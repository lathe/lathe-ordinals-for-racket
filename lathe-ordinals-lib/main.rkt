#lang parendown racket/base

; lathe-ordinals
;
; A representation of ordinal numbers, currently supporting the
; ordinals up to epsilon zero (the first infinite ordinal which is a
; fixed point of exponentiation).

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
  *list/c -> ->* any/c flat-contract? list/c listof or/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/maybe
  just maybe/c maybe-map nothing)
(require #/only-in lathe-comforts/list
  list-each list-foldl list-foldr list-map nat->maybe)
(require #/only-in lathe-comforts/struct struct-easy)

; TODO: Document all of these exports.
(provide
  onum<=e0? onum<e0?
  onum<=omega? onum<omega?
  
  ; TODO: Decide whether to provide these. Maybe provide this
  ; functionality in another way, like a logarithm operation.
  ;
  ; cnf->onum
  onum->cnf
  
  onum-compare onum<? onum>? onum<=? onum>=? onum</c onum<=/c
  onum-omega (rename-out [-onum-e0 onum-e0])
  onum-plus1
  onum-plus-list onum-plus
  onum-drop1
  onum-drop
  onum-times-list onum-times
  onum-untimes
  onum->limit-plus-finite
  onum-pred-maybe
  onum-pow-list onum-pow
)



; Epsilon zero, the first ordinal that can't be expressed in
; Cantor normal form.
(struct-easy (onum-e0) #:equal)

; The ordinals greater than or equal to omega (the first infinite
; ordinal) and less than epsilon zero, represented in Cantor normal
; form.
(struct-easy (onum-cnf base-omega-expansion) #:equal
  (#:guard-easy
    (unless (list? base-omega-expansion)
      (error "Expected base-omega-expansion to be a list"))
    (mat base-omega-expansion (list)
      (error "Expected base-omega-expansion to represent an infinite ordinal, not zero")
    #/mat base-omega-expansion (list #/list 0 n)
      (error "Expected base-omega-expansion to represent an infinite ordinal")
    #/list-foldl (onum-e0) base-omega-expansion #/fn prev-power term
      (expect term (list power coefficient)
        (error "Expected each term to be a two-element list")
      #/expect (onum<e0? power) #t
        (error "Expected each power to be an ordinal less than epsilon zero")
      #/expect (exact-positive-integer? coefficient) #t
        (error "Expected each coefficient to be an exact positive integer")
      #/expect (onum<? power prev-power) #t
        (error "Expected each power to be strictly less than the last")
        power))))

(define/contract (onum<e0? x)
  (-> any/c boolean?)
  (or (natural? x) (onum-cnf? x)))

(define/contract (onum<=e0? x)
  (-> any/c boolean?)
  (or (onum<e0? x) (onum-e0? x)))

(define/contract (onum->cnf n)
  (-> onum<e0? #/listof #/list/c onum<e0? exact-positive-integer?)
  (mat n 0 (list)
  #/mat n (onum-cnf n) n
  #/list #/list 0 n))

(define/contract (cnf->onum base-omega-expansion)
  (-> (listof #/list/c onum<e0? exact-positive-integer?) onum<e0?)
  (mat base-omega-expansion (list) 0
  #/mat base-omega-expansion (list #/list 0 n) n
  ; TODO: See if we should check that the powers are strictly
  ; decreasing here. We already do in the `onum-cnf` constructor
  ; guard, but we'll probably want to do it here with a more tailored
  ; error message if this is user-facing.
  #/onum-cnf base-omega-expansion))

; TODO: Put this in Lathe Comforts.
(define/contract (nat-compare a b)
  (-> natural? natural? #/or/c '< '= '>)
  (if (< a b) '<
  #/if (= a b) '=
    '>))

(define/contract (onum-compare a b)
  (-> onum<=e0? onum<=e0? #/or/c '< '= '>)
  (mat a (onum-e0) (mat b (onum-e0) '= '>)
  #/mat b (onum-e0) '<
  #/w- a (onum->cnf a)
  #/w- b (onum->cnf b)
  #/w-loop next a a b b
    (expect a (cons a-first a-rest) (mat b (list) '= '<)
    #/expect b (cons b-first b-rest) '>
    #/dissect a-first (list a-power a-coefficient)
    #/dissect b-first (list b-power b-coefficient)
    #/w- power-comparison (onum-compare a-power b-power)
    #/expect power-comparison '= power-comparison
    #/w- coefficient-comparison
      (nat-compare a-coefficient b-coefficient)
    #/expect coefficient-comparison '= coefficient-comparison
    #/next a-rest b-rest)))

(define/contract (onum<? a b)
  (-> onum<=e0? onum<=e0? boolean?)
  (eq? '< #/onum-compare a b))

(define/contract (onum>? a b)
  (-> onum<=e0? onum<=e0? boolean?)
  (eq? '> #/onum-compare a b))

(define/contract (onum<=? a b)
  (-> onum<=e0? onum<=e0? boolean?)
  (not #/onum>? a b))

(define/contract (onum>=? a b)
  (-> onum<=e0? onum<=e0? boolean?)
  (not #/onum<? a b))

(define/contract (onum</c n)
  (-> onum<=e0? flat-contract?)
  ; TODO: Don't use a plain lambda like this for a contract.
  (fn v #/and (onum<=e0? v) (onum<? v n)))

(define/contract (onum<=/c n)
  (-> onum<=e0? flat-contract?)
  ; TODO: Don't use a plain lambda like this for a contract.
  (fn v #/and (onum<=e0? v) (onum<=? v n)))

(define/contract (onum-omega)
  (-> onum<e0?)
  (onum-cnf #/list #/list 1 1))

(define/contract (onum<=omega? x)
  (-> any/c boolean?)
  (or (natural? x) (equal? x #/onum-omega)))

(define/contract (onum<omega? x)
  (-> any/c boolean?)
  (natural? x))

; NOTE: This is just like `onum-e0` except for its interaction with
; `struct-constructor-procedure?`.
(define/contract (-onum-e0)
  (-> onum<=e0?)
  (onum-e0))

; This is increment by way of addition on the left. We're finding
; `(onum-plus 1 n)`.
(define/contract (onum-plus1 n)
  (-> onum<=e0? onum<=e0?)
  (if (natural? n)
    (+ 1 n)
    n))

; TODO: Put this in Lathe Comforts.
(define/contract (list-rev-onto source target)
  (-> list? any/c any/c)
  (expect source (cons first rest) target
  #/list-rev-onto rest #/cons first target))

(define/contract (onum-plus-binary a b)
  (-> onum<e0? onum<=e0? onum<=e0?)
  (mat b (onum-e0) (onum-e0)
  #/w- b-expansion (onum->cnf b)
  #/expect b-expansion (cons b-first b-rest) a
  #/dissect b-first (list b-power b-coefficient)
  #/w- a-expansion (onum->cnf a)
  #/w-loop next rev-a (reverse a-expansion)
    (expect rev-a (cons rev-a-first rev-a-rest) b
    #/dissect rev-a-first (list a-power a-coefficient)
    #/w- comparison (onum-compare a-power b-power)
    #/mat comparison '< (next rev-a-rest)
    #/mat comparison '> (cnf->onum #/list-rev-onto rev-a b-expansion)
    #/cnf->onum
    #/list-rev-onto rev-a-rest
    #/cons (list b-power #/+ a-coefficient b-coefficient) b-rest)))

(define/contract (onum-plus-list ns)
  (-> (or/c (list/c) #/*list/c onum<e0? onum<=e0?) onum<=e0?)
  (list-foldr ns 0 #/fn a b #/onum-plus-binary a b))

(define/contract (onum-plus . ns)
  (->* () #:rest (or/c (list/c) #/*list/c onum<e0? onum<=e0?)
    onum<=e0?)
  (onum-plus-list ns))

; This is decrement by way of left subtraction. We're finding the
; value `result` such that `(equal? (onum-plus 1 result) n)`,
; if it exists. It exists as long as `n` is 1 or greater.
(define/contract (onum-drop1 n)
  (-> onum<=e0? #/maybe/c onum<=e0?)
  (if (natural? n)
    (- n 1)
    n))

; This is left subtraction. We're finding the value `result` such that
; `(equal? (onum-plus amount result) n)`, if it exists. It exists as
; long as `amount` is less than or equal to `n`.
(define/contract (onum-drop amount n)
  (-> onum<=e0? onum<=e0? #/maybe/c onum<=e0?)
  (mat n (onum-e0) (just #/mat amount (onum-e0) 0 (onum-e0))
  #/mat amount (onum-e0) (nothing)
  #/w- amount-expansion (onum->cnf amount)
  #/w- n-expansion (onum->cnf amount)
  #/w-loop next
    amount-expansion amount-expansion
    n-expansion n-expansion
    
    (expect n-expansion (cons n-first n-rest)
      (mat amount-expansion (list) (just 0)
      #/nothing)
    #/dissect n-first (list n-power n-coefficient)
    #/expect amount-expansion (cons amount-first amount-rest)
      (just #/cnf->onum n-expansion)
    #/dissect amount-first (list amount-power amount-coefficient)
    #/w- power-comparison (onum-compare amount-power n-power)
    #/mat power-comparison '> (nothing)
    #/mat power-comparison '< (next amount-rest n-expansion)
    #/w- coefficient-comparison
      (nat-compare amount-coefficient n-coefficient)
    #/mat coefficient-comparison '> (nothing)
    #/mat coefficient-comparison '= (next amount-rest n-rest)
    #/cnf->onum
    #/cons (list n-power #/- n-coefficient amount-coefficient)
      n-rest)))

(define/contract (onum-times-binary a b)
  (-> onum<e0? onum<=e0? onum<=e0?)
  (mat b (onum-e0) (onum-e0)
  #/w- a (onum->cnf a)
  #/w- b (onum->cnf b)
  #/expect a (cons a-first a-rest) 0
  #/dissect a-first (list a-power a-coefficient)
  #/onum-plus-list
  #/list-map b #/dissectfn (list b-power b-coefficient)
    (cnf->onum #/cons
      (list
        (onum-plus-binary a-power b-power)
        (mat b-power 0
          (* a-coefficient b-coefficient)
          b-coefficient))
      a-rest)))

(define/contract (onum-times-list ns)
  (-> (or/c (list/c) #/*list/c onum<e0? onum<=e0?) onum<=e0?)
  (list-foldr ns 1 #/fn a b #/onum-times-binary a b))

(define/contract (onum-times . ns)
  (->* () #:rest (or/c (list/c) #/*list/c onum<e0? onum<=e0?)
    onum<=e0?)
  (onum-times-list ns))

; TODO: All the procedures in this module need to be tested, but this
; one is especially tricky. Be sure to test this one.
;
; TODO: See if there's a better name for this than `onum-untimes`.
; Names connoting division would be great, but the order of arguments
; is the opposite as it usually is for division notation.
;
; This is left division. We're finding the value
; `(list quotient remainder)` such that `(onum<? remainder amount)`
; and `(equal? (onum-plus (onum-times amount quotient) remainder) n)`,
; if it exists. It exists as long as `amount` is nonzero.
(define/contract (onum-untimes amount n)
  (-> onum<=e0? onum<=e0? #/maybe/c #/list/c onum<=e0? onum<e0?)
  (mat n (onum-e0)
    (just #/list
      (mat amount (onum-e0) 1 (onum-e0))
      0)
  #/if (onum<? n amount) (just #/list 0 n)
  #/w- amount-expansion (onum->cnf amount)
  #/expect amount-expansion (cons amount-first amount-rest) (nothing)
  #/dissect amount-first (list amount-power amount-coefficient)
  #/dissect (onum->cnf n) (cons (list n-power n-coefficient) n-rest)
  
  ; OPTIMIZATION: If both ordinals are finite, we can use Racket's
  ; `quotient/remainder` on the corresponding Racket integers.
  ;
  ; TODO: Test this without the optimization in place, for confidence
  ; that it's not changing the behavior.
  ;
  #/mat n-power 0
    (let ()
      (define-values (q r)
        (quotient/remainder n-coefficient amount-coefficient))
      (just #/list q r))
  
  #/dissect
    (if (equal? amount-power n-power)
      (w- q-first-1 (quotient n-coefficient amount-coefficient)
      #/dissect (nat->maybe q-first-1) (just q-first-2)
      #/list q-first-1 q-first-2)
      (w- q-first
        (onum->cnf
        #/list (onum-drop amount-power n-power) n-coefficient)
      ; NOTE: The second element of this list doesn't matter because
      ; the first one is guaranteed to multiply to a value less than
      ; `n`.
      #/list q-first q-first))
    (list q-first-1 q-first-2)
  #/dissect
    
    ; We use a long division method where we divide the most
    ; significant digits, attempt to use that quotient of the digits
    ; as the most significant digit of the overall quotient, and if
    ; that's too big, we fall back to using the value one less than
    ; that.
    ;
    ; We don't have to worry about what "one less" means for infinite
    ; ordinals: If the quotient of the digits is infinite (because the
    ; digits are associated with different powers of omega), the first
    ; guess will always be small enough to work in the overall
    ; quotient. Specifically, the digit quotient is equal to the digit
    ; taken from `n`, and when it's multiplied by the `amount` digit
    ; on the left, nothing will happen; hence the subtraction will
    ; just remove that digit from `n`.
    ;
    (mat (onum-drop (onum-times-binary amount q-first-1) n)
      (just n-rest-1)
      (list q-first-1 n-rest-1)
    #/dissect (onum-drop (onum-times-binary amount q-first-2) n)
      (just n-rest-2)
      (list q-first-2 n-rest-2))
    (list q-first n-rest)
  #/dissect (onum-untimes amount n-rest) (just #/list q-rest r)
  #/just #/list (onum-plus-binary q-first q-rest) r))

(define/contract (onum->limit-plus-finite n)
  (-> onum<=e0? #/list/c onum<=e0? natural?)
  (dissect (onum-untimes (onum-omega) n)
    (just #/list limit-part-div-omega finite-part)
  #/list (onum-times (onum-omega) limit-part-div-omega) finite-part))

(define/contract (onum-pred-maybe n)
  (-> onum<=e0? #/maybe/c onum<e0?)
  (dissect (onum->limit-plus-finite n) (list limit-part finite-part)
  #/maybe-map (nat->maybe finite-part) #/fn finite-part
    (onum-plus limit-part finite-part)))

(define/contract (onum-pow-by-nat base exponent)
  (-> onum<e0? natural? onum<e0?)
  (mat exponent 0 1
  ; We proceed by the method of exponentiation by parts: We recur on
  ; half the exponent and use that result twice in order to save on
  ; the overall number of multiplications performed.
  #/let-values
    ([(half-exponent parity) (quotient/remainder exponent 2)])
  #/w- sqrt-near-result (onum-pow-by-nat half-exponent)
  #/w- near-result
    (onum-times-binary sqrt-near-result sqrt-near-result)
  #/mat parity 0
    near-result
    (onum-times base near-result)))

; TODO: This one's also tricky. Let's make sure to test this.
(define/contract (onum-pow-binary base exponent)
  (-> onum<e0? onum<e0? onum<e0?)
  (mat exponent 0 1
  #/w- base-expansion (onum->cnf base)
  #/expect base-expansion (cons base-first base-rest) 0
  #/dissect base-first (list base-first-power base-first-coefficient)
  #/dissect (onum->limit-plus-finite exponent)
    (list exponent-limit-part exponent-finite-part)
  #/onum-times
    (cnf->onum
    #/list #/list (onum-times base-first-power exponent-limit-part) 1)
    (onum-pow-by-nat base exponent-finite-part)))

(define/contract (onum-pow-list ns)
  (-> (listof onum<e0?) onum<e0?)
  (list-foldr ns 1 #/fn a b #/onum-pow-binary a b))

(define/contract (onum-pow . ns)
  (->* () #:rest (listof onum<e0?) onum<e0?)
  (onum-pow-list ns))

; TODO: See if we can define an `onum-log` operation that's related to
; `onum-pow` the way `onum-untimes` is related to `onum-times` and
; `onum-drop` is related to `onum-plus`. This operation would find
; the value `(list exponent factor term)` that solves
;
;   (equal? n
;     (onum-plus (onum-times (onum-pow amount exponent) factor) term))
;
; for a given `amount` and a given `n`, such that
; `(onum<? factor amount)` and
; `(onum<? term (onum-pow amount exponent))`.
;
; It looks like just such an operation is desccribed here, along with
; a greatest common divisor operation and prime factorization, which
; would also be good:
;
; https://www.maplesoft.com/products/maple/new_features/maple19/Ordinals_Maple2015.pdf

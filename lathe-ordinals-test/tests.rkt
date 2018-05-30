#lang parendown racket/base

; lathe-ordinals/tests
;
; Unit tests.

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


(require rackunit)

;(require lathe-ordinals/olist)
(require lathe-ordinals/onum)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(check-equal? (onum? onum-zero) #t
  "Test that `onum?` recognizes an actual onum")


; Tests corresponding to documentation examples

(define omega-plus-four (onum-plus onum-omega (nat->onum 4)))
(define square-of-that
  (onum-times omega-plus-four omega-plus-four))
(onum-base-omega-expansion omega-plus-four)
(onum-base-omega-expansion square-of-that)

(check-equal?
  (onum-base-omega-expansion omega-plus-four)
  (list (list onum-one 1) (list onum-zero 4)))

(check-equal?
  (onum-base-omega-expansion square-of-that)
  (list (list (nat->onum 2) 1) (list onum-one 4) (list onum-zero 4)))

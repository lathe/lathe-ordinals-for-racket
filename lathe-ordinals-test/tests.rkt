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

(require lathe-ordinals)
;(require lathe-ordinals/olist)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(check-equal? (onum<=e0? 0) #t
  "Test that `onum<=e0?` recognizes zero")


; Tests corresponding to documentation examples

(define omega-plus-four (onum-plus (onum-omega) 4))
(define square-of-that (onum-times omega-plus-four omega-plus-four))

(check-equal?
  (onum->cnf omega-plus-four)
  (list (list 1 1) (list 0 4)))

(check-equal?
  (onum->cnf square-of-that)
  (list (list 2 1) (list 1 4) (list 0 4)))

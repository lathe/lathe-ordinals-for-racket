#lang info

(define collection "lathe-ordinals")

(define deps (list "base"))
(define build-deps
  (list
    "lathe-comforts-lib"
    "lathe-ordinals-lib"
    "parendown-lib"
    "racket-doc"
    "scribble-lib"))

(define scribblings
  (list (list "scribblings/lathe-ordinals.scrbl" (list))))

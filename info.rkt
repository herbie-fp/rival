#lang info

(define collection "rival")
(define version "1.7")

;; Packaging information

(define pkg-desc "Interval arithmetic for real computation")
(define pkg-authors '("Pavel Panchekha" "Oliver Flatt"))

(define compile-omit-paths '("infra"))

;; Dependencies
(define deps '(("base" #:version "8.0") "math-lib" "rackunit-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc" "math-doc" "sandbox-lib"))
(define scribblings '(("rival.scrbl")))


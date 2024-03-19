#lang info

(define collection "rival")
(define version "1.7")
(define license "MIT License")

;; Packaging information

(define pkg-desc "Interval arithmetic for real computation")
(define pkg-authors '("Pavel Panchekha" "Oliver Flatt" "Artem Yadrov"))

(define compile-omit-paths '("infra"))
(define test-omit-paths '("infra"))

;; Dependencies
(define deps '(("base" #:version "8.0") "math-lib" "rackunit-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc" "math-doc" "sandbox-lib"))
(define scribblings '(("rival.scrbl")))


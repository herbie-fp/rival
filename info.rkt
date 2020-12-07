#lang info

(define collection "rival")
(define version "1.4")

;; Packaging information

(define pkg-desc "Interval arithmetic for real computation")
(define pkg-authors
  '("Pavel Panchekha" "Oliver Flatt"))

;; Dependencies
(define deps '(("base" #:version "7.0") "math-lib" "rackunit-lib" "biginterval"))
(define build-deps '("rackunit-lib"))

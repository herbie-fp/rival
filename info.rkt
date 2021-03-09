#lang info

(define collection "rival")
(define version "1.4")

;; Packaging information

(define pkg-desc "Interval arithmetic for real computation")
(define pkg-authors
  '("Pavel Panchekha" "Oliver Flatt"))

(define compile-omit-paths '("infra"))

;; Dependencies
(define deps '(("base" #:version "7.0") "math-lib" "rackunit-lib"))
(define build-deps '("rackunit-lib"))


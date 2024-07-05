#lang racket/base

(require "eval/machine.rkt")
(require math/bigfloat math/flonum)
(provide flonum-discretization boolean-discretization bf-discretization)

(define flonum-discretization
  (discretization bigfloat->flonum (compose abs flonums-between)))

(define boolean-discretization
  (discretization values (lambda (x y) (if (eq? x y) 0 2))))

(define (bf-discretization [precision #f])
  (define n (or precision (bf-precision)))
  (discretization
   (lambda (x) (parameterize ([bf-precision n]) (bfcopy x)))
   (lambda (x y) (parameterize ([bf-precision n]) (abs (bigfloats-between x y))))))

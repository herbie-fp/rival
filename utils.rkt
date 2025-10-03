#lang racket/base

(require "mpfr.rkt"
         "eval/machine.rkt")
(require math/flonum)
(provide flonum-discretization
         boolean-discretization
         bf-discretization)

(define flonum-discretization (discretization 53 bigfloat->flonum (compose abs flonums-between)))

(define boolean-discretization (discretization 53 values (lambda (x y) (if (eq? x y) 0 2))))

(define min-exponent (mpfr-exp (bfnext 0.bf)))

(define (bf-discretization [precision #f])
  (define n (or precision (bf-precision)))
  (discretization n
                  (lambda (x)
                    (parameterize ([bf-precision n])
                      (if (and (= (mpfr-exp x) min-exponent) (equal? (bigfloats-between 0.bf x) 1))
                          (bf 0)
                          (bfcopy x))))
                  (lambda (x y)
                    (parameterize ([bf-precision n])
                      (abs (bigfloats-between x y))))))

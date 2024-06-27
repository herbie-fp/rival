#lang racket

(require "core.rkt" "../mpfr.rkt")
(provide ival-pow ival-pow2)

(define (classify-pos-ival-1 x) ;; Assumes x positive
  (define x.lo (ival-lo-val x))
  (cond
    [(>= (mpfr-exp (ival-lo-val x)) 1) 1]
    [(< (mpfr-exp (ival-hi-val x)) 1) -1]
    [else 0]))

(define (eppow a-endpoint b-endpoint a-class b-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bfexpt (list a b)))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bf=? a 1.bf))
       (and a! (bfzero? a) (not (= b-class 0)))
       (and a! (bfinfinite? a) (not (= b-class 0)))
       (and b! (bfzero? b))
       (and b! (bfinfinite? b) (not (= a-class 0))))))

(define (ival-copy-movability i1 i2)
  (ival (endpoint (ival-lo-val i1) (ival-lo-fixed? i2))
        (endpoint (ival-hi-val i1) (ival-hi-fixed? i2))
        (ival-err? i1)
        (ival-err i1)))

(define (ival-pow-pos x y)
  ;; Assumes x is positive; code copied from ival-mult
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define x-class (classify-pos-ival-1 x))
  (define y-class (classify-ival y))

  (define (mk-pow a b c d)
    (match-define (endpoint lo lo!) (rnd 'down eppow a b x-class y-class))
    (match-define (endpoint hi hi!) (rnd 'up   eppow c d x-class y-class))

    (define-values (real-lo! real-hi!)
      (cond
        [(or (bfzero? lo) (bfinfinite? hi))
         (match-define (ival (endpoint tlo tlo!) (endpoint thi thi!) _ _)
           (ival-mult (ival b d #f #f)
                      (ival (endpoint (rnd 'zero bflog2 (endpoint-val a)) (endpoint-immovable? a))
                            (endpoint (rnd 'zero bflog2 (endpoint-val c)) (endpoint-immovable? c))
                            #f #f)))

         (define hi-bar exp2-overflow-threshold)
         ;; Important: exp2-overflow-threshold is an exact power of 2, so we can use >=
         (define must-overflow
           (and (bfinfinite? hi) (= (mpfr-sign tlo) 1) (>= (mpfr-exp tlo) (mpfr-exp hi-bar))))
         (define must-underflow
           (and (bfzero? lo) (= (mpfr-sign thi) -1) (>= (mpfr-exp thi) (mpfr-exp hi-bar))))

         (define real-lo! (or lo! must-underflow (and (bfzero? lo) tlo!)))
         (define real-hi! (or hi! must-underflow must-overflow (and (bfinfinite? hi) thi!)))

         #|
         ;; BEGIN DEBUGGING CODE
         (define other-option (ival-exp (ival-mult y (ival-log x))))
         (define best-lo! (ival-lo-fixed? other-option))
         (define best-hi! (ival-hi-fixed? other-option))

         (unless (and (eq? real-lo! best-lo!) (eq? real-hi! best-hi!))
           (eprintf "Bad flags: [~a ~a] us vs [~a ~a] best\n" real-lo! best-lo! real-hi! best-hi!)
           (eprintf "  pow(~a, ~a)\n" x y)
           (eprintf "a: ~a\nc: ~a\n\n" a c)
           (eprintf "tlo: ~a\nthi: ~a\n" tlo thi)
           (eprintf "must: uflow ~a, oflow: ~a\n\n" must-underflow must-overflow)
           (eprintf "hi-bar: ~a\n" hi-bar)
           (error "Exiting"))
         ;; END DEBUGGING CODE
         |#

         (values real-lo! real-hi!)]
        [else
         (values lo! hi!)]))

    (ival (endpoint lo real-lo!) (endpoint hi real-hi!)
          (or xerr? yerr? (and (bfzero? (endpoint-val xlo)) (not (= y-class 1))))
          (or xerr yerr (and (bfzero? (endpoint-val xhi)) (= y-class -1)))))

  (match* (x-class y-class)
    [( 1  1) (mk-pow xlo ylo xhi yhi)]
    [( 1  0) (mk-pow xhi ylo xhi yhi)]
    [( 1 -1) (mk-pow xhi ylo xlo yhi)]
    [( 0  1) (mk-pow xlo yhi xhi yhi)]
    [( 0 -1) (mk-pow xhi ylo xlo ylo)]
    [(-1  1) (mk-pow xlo yhi xhi ylo)]
    [(-1  0) (mk-pow xlo yhi xlo ylo)]
    [(-1 -1) (mk-pow xhi yhi xlo ylo)]
    [( 0  0) ;; Special case
     (ival-union (mk-pow xlo yhi xhi yhi) (mk-pow xhi ylo xlo ylo))]))


(define (ival-pow-neg x y)
  ;; Assumes x is negative
  (if (bf=? (ival-lo-val y) (ival-hi-val y))
      (if (bfinteger? (ival-lo-val y))
          ; If y is an integer point interval, there's no error,
          ; because it's always valid to raise to an integer power.
          (if (bfodd? (ival-lo-val y))
              (ival-neg (ival-pow-pos (ival-exact-fabs x) y)) ; Use fabs in case of [x, 0]
              (ival-pow-pos (ival-exact-fabs x) y))
          ; If y is non-integer point interval, it must be an even
          ; fraction (because all bigfloats are) so we always error
          ival-illegal)
      ; Moreover, if we have (-x)^y, that's basically x^y U -(x^y).
      (let ([pospow (ival-pow-pos (ival-exact-fabs x) y)])
        (ival-then (ival-assert ival-maybe) (ival-union (ival-neg pospow) pospow)))))

(define (ival-pow2 x)
  ((monotonic->ival (lambda (x) (bfmul x x))) (ival-exact-fabs x)))

(define (ival-pow x y)
  (match (classify-ival x)
   [-1 (ival-pow-neg x y)]
   [1 (ival-pow-pos x y)]
   [0
    (define-values (neg pos) (ival-split x 0.bf))
    (ival-union (ival-pow-neg neg y) (ival-pow-pos pos y))]))

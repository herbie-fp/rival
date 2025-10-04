#lang racket

(require "arith.rkt"
         "core.rkt"
         "../mpfr.rkt")
(provide ival-sin
         ival-cos
         ival-tan
         ival-cosu
         ival-sinu
         ival-tanu)

(define *rival-precision* (make-parameter (expt 2 20)))

(define (eptrig-min! out mpfr-fn! a-endpoint b-endpoint rnd)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define out-a (mpfr-new! (bf-precision)))
  (define out-b (mpfr-new! (bf-precision)))
  (define exact-a? (= 0 (mpfr-fn! out-a a rnd)))
  (define exact-b? (= 0 (mpfr-fn! out-b b rnd)))
  (mpfr-set-prec! out (bf-precision))
  (cond
    [(bflt? out-a out-b)
     (mpfr-set! out out-a rnd)
     (endpoint out (and a! exact-a?))]
    [(bflt? out-b out-a)
     (mpfr-set! out out-b rnd)
     (endpoint out (and b! exact-b?))]
    [else
     (mpfr-set! out out-a rnd)
     (endpoint out (or (and a! exact-a?) (and b! exact-b?)))]))

(define (eptrig-max! out mpfr-fn! a-endpoint b-endpoint rnd)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define out-a (mpfr-new! (bf-precision)))
  (define out-b (mpfr-new! (bf-precision)))
  (define exact-a? (= 0 (mpfr-fn! out-a a rnd)))
  (define exact-b? (= 0 (mpfr-fn! out-b b rnd)))
  (mpfr-set-prec! out (bf-precision))
  (cond
    [(bfgt? out-a out-b)
     (mpfr-set! out out-a rnd)
     (endpoint out (and a! exact-a?))]
    [(bfgt? out-b out-a)
     (mpfr-set! out out-b rnd)
     (endpoint out (and b! exact-b?))]
    [else
     (mpfr-set! out out-a rnd)
     (endpoint out (or (and a! exact-a?) (and b! exact-b?)))]))

(define (range-reduce-floor x divisor-bf prec)
  (define saved-prec (bf-precision))
  (bf-precision prec)
  (define divisor-ival (mk-ival divisor-bf))
  (define result-ival (ival-floor (ival-div x divisor-ival)))
  (bf-precision saved-prec)
  result-ival)

(define (range-reduce-round x divisor-bf prec)
  (define saved-prec (bf-precision))
  (bf-precision prec)
  (define divisor-ival (mk-ival divisor-bf))
  (define result-ival (ival-round (ival-div x divisor-ival)))
  (bf-precision saved-prec)
  result-ival)

(define (classify-ival-periodic x period)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (define period/4 (integer-length (exact-floor (/ period 4))))
  (define lo-exp (mpfr-exp xlo))
  (define hi-exp (mpfr-exp xhi))
  (define lo-ulp (- lo-exp (bigfloat-precision xlo)))
  (define hi-ulp (- hi-exp (bigfloat-precision xhi)))
  (cond
    [(or (bfinfinite? xlo) (bfinfinite? xhi)) 'too-wide]
    [(and (< lo-exp period/4) (< hi-exp period/4)) 'near-0]
    [(or (> lo-ulp 0) (> hi-ulp 0)) (if (bf=? xlo xhi) 'range-reduce 'too-wide)]
    [else 'range-reduce]))

(define (range-reduce-precision xlo xhi)
  (min (*rival-precision*)
       (max (bf-precision)
            (max (+ (bigfloat-exponent xlo) (bigfloat-precision xlo) (bigfloat-precision xlo))
                 (+ (bigfloat-exponent xhi) (bigfloat-precision xhi) (bigfloat-precision xhi))))))

(define (ival-cosu n)
  (define (ival-cosu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
    (match (classify-ival-periodic x n)
      ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
      ['near-0
       (define (mpfr-cosu-n! out a rnd)
         (mpfr-cosu! out a n rnd))
       (match (classify-ival x)
         [-1 ((monotonic-mpfr mpfr-cosu-n!) x)]
         [1 ((comonotonic-mpfr mpfr-cosu-n!) x)]
         [else
          (define out (new-ival))
          (ival (eptrig-min! (ival-lo-val out) mpfr-cosu-n! (ival-lo x) (ival-hi x) 'down)
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))])]
      ['range-reduce
       (define prec (range-reduce-precision xlo xhi))
       (define divisor-bf (mpfr-new! prec))
       (mpfr-set! divisor-bf (bf (/ n 2)) 'nearest)
       (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-floor x divisor-bf prec))
       (define (mpfr-cosu-n! out a rnd)
         (mpfr-cosu! out a n rnd))
       (cond
         [(and (bf=? a b) (bfeven? a)) ((comonotonic-mpfr mpfr-cosu-n!) x)]
         [(and (bf=? a b) (bfodd? a)) ((monotonic-mpfr mpfr-cosu-n!) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (define out (new-ival))
          (ival (endpoint -1.bf #f)
                (eptrig-max! (ival-hi-val out) mpfr-cosu-n! (ival-lo x) (ival-hi x) 'up)
                (ival-err? x)
                (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (define out (new-ival))
          (ival (eptrig-min! (ival-lo-val out) mpfr-cosu-n! (ival-lo x) (ival-hi x) 'down)
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))]
         [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))
  (if bfcosu ival-cosu #f))

(define (ival-cos x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match (classify-ival-periodic x (* 2 pi))
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0
     (match (classify-ival x)
       [-1 ((monotonic-mpfr mpfr-cos!) x)]
       [1 ((comonotonic-mpfr mpfr-cos!) x)]
       [else
        (define out (new-ival))
        (ival (eptrig-min! (ival-lo-val out) mpfr-cos! (ival-lo x) (ival-hi x) 'down)
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))])]
    ['range-reduce
     (define prec (range-reduce-precision xlo xhi))
     (define pi-bf (mpfr-new! prec))
     (mpfr-const-pi! pi-bf 'nearest)
     (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-floor x pi-bf prec))
     (cond
       [(and (bf=? a b) (bfeven? a)) ((comonotonic-mpfr mpfr-cos!) x)]
       [(and (bf=? a b) (bfodd? a)) ((monotonic-mpfr mpfr-cos!) x)]
       [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
        (define out (new-ival))
        (ival (endpoint -1.bf #f)
              (eptrig-max! (ival-hi-val out) mpfr-cos! (ival-lo x) (ival-hi x) 'up)
              (ival-err? x)
              (ival-err x))]
       [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
        (define out (new-ival))
        (ival (eptrig-min! (ival-lo-val out) mpfr-cos! (ival-lo x) (ival-hi x) 'down)
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))]
       [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))

(define (ival-sinu n)
  (define (ival-sinu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

    (define (mpfr-sinu-n! out a rnd)
      (mpfr-sinu! out a n rnd))
    (match (classify-ival-periodic x n)
      ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
      ['near-0 ((monotonic-mpfr mpfr-sinu-n!) x)]
      ['range-reduce
       (define prec (range-reduce-precision xlo xhi))
       (define divisor-bf (mpfr-new! prec))
       (mpfr-set! divisor-bf (bf (/ n 2)) 'nearest)
       (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-round x divisor-bf prec))
       (cond
         [(and (bf=? a b) (bfodd? a)) ((comonotonic-mpfr mpfr-sinu-n!) x)]
         [(and (bf=? a b) (bfeven? a)) ((monotonic-mpfr mpfr-sinu-n!) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (define out (new-ival))
          (ival (endpoint -1.bf #f)
                (eptrig-max! (ival-hi-val out) mpfr-sinu-n! (ival-lo x) (ival-hi x) 'up)
                (ival-err? x)
                (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (define out (new-ival))
          (ival (eptrig-min! (ival-lo-val out) mpfr-sinu-n! (ival-lo x) (ival-hi x) 'down)
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))]
         [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))
  (if bfsinu ival-sinu #f))

(define (ival-sin x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

  (match (classify-ival-periodic x (* 2 pi))
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0 ((monotonic-mpfr mpfr-sin!) x)]
    ['range-reduce
     (define prec (range-reduce-precision xlo xhi))
     (define pi-bf (mpfr-new! prec))
     (mpfr-const-pi! pi-bf 'nearest)
     (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-round x pi-bf prec))
     (cond
       [(and (bf=? a b) (bfodd? a)) ((comonotonic-mpfr mpfr-sin!) x)]
       [(and (bf=? a b) (bfeven? a)) ((monotonic-mpfr mpfr-sin!) x)]
       [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
        (define out (new-ival))
        (ival (endpoint -1.bf #f)
              (eptrig-max! (ival-hi-val out) mpfr-sin! (ival-lo x) (ival-hi x) 'up)
              (ival-err? x)
              (ival-err x))]
       [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
        (define out (new-ival))
        (ival (eptrig-min! (ival-lo-val out) mpfr-sin! (ival-lo x) (ival-hi x) 'down)
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))]
       [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))

(define (ival-tanu n)
  (define (ival-tanu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
    (define (mpfr-tanu-n! out a rnd)
      (mpfr-tanu! out a n rnd))

    (match (classify-ival-periodic x (/ n 2))
      ['too-wide (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr)]
      ['near-0 ((monotonic-mpfr mpfr-tanu-n!) x)]
      ['range-reduce
       (define prec (range-reduce-precision xlo xhi))
       (define divisor-bf (mpfr-new! prec))
       (mpfr-set! divisor-bf (bf (/ n 2)) 'nearest)
       (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-round x divisor-bf prec))

       (if (bf=? a b) ; Same period
           ((monotonic-mpfr mpfr-tanu-n!) x)
           (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr))]))
  (if bftanu ival-tanu #f))

(define (ival-tan x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

  (match (classify-ival-periodic x pi)
    ['too-wide (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr)]
    ['near-0 ((monotonic-mpfr mpfr-tan!) x)]
    ['range-reduce
     (define prec (range-reduce-precision xlo xhi))
     (define pi-bf (mpfr-new! prec))
     (mpfr-const-pi! pi-bf 'nearest)
     (match-define (ival (endpoint a _) (endpoint b _) _ _) (range-reduce-round x pi-bf prec))

     (if (bf=? a b) ; Same period
         ((monotonic-mpfr mpfr-tan!) x)
         (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr))]))

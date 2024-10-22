#lang racket/base

(require racket/match
         racket/function
         racket/list)

(require "../mpfr.rkt"
         "../ops/all.rkt"
         "machine.rkt")

(provide get-bounds
         get-slack)

(define (get-slack [iter (*sampling-iteration*)])
  (match iter
    [0 0]
    [1 512]
    [2 1024]
    [3 2048]
    [4 4096]
    [5 8192]))

(define (prev-iter)
  (- (*sampling-iteration*) 1))

(define (crosses-zero? x)
  (not (equal? (mpfr-sign (ival-lo x)) (mpfr-sign (ival-hi x)))))

; We assume the interval x is valid. Critical not to take mpfr-exp of inf or 0,
; the results are platform-dependant
(define (maxlog x #:no-slack [no-slack #f])
  (define iter
    (if no-slack
        0
        (*sampling-iteration*)))
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (cond
    ; x = [-inf, inf]
    [(and (bfinfinite? hi) (bfinfinite? lo)) (get-slack iter)]
    [(bfinfinite? hi) (+ (max (mpfr-exp lo) 0) (get-slack iter))] ; x = [..., inf]
    [(bfinfinite? lo) (+ (max (mpfr-exp hi) 0) (get-slack iter))] ; x = [-inf, ...]
    [else
     (+ (max (mpfr-exp lo) (mpfr-exp hi)) 1)])) ; x does not contain inf, safe with respect to 0.bf

(define (minlog x #:no-slack [no-slack #f])
  (define iter
    (if no-slack
        0
        (*sampling-iteration*)))
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (cond
    ; x = [0.bf, ...]
    [(bfzero? lo)
     (if (bfinfinite? hi)
         (- (get-slack iter))
         (- (min (mpfr-exp hi) 0) (get-slack iter)))]
    ; x = [..., 0.bf]
    [(bfzero? hi)
     (if (bfinfinite? lo)
         (- (get-slack iter))
         (- (min (mpfr-exp lo) 0) (get-slack iter)))]
    [(crosses-zero? x) ; x = [-..., +...]
     (cond
       [(and (bfinfinite? hi) (bfinfinite? lo)) (- (get-slack iter))]
       [(bfinfinite? hi) (- (min (mpfr-exp lo) 0) (get-slack iter))]
       [(bfinfinite? lo) (- (min (mpfr-exp hi) 0) (get-slack iter))]
       [else (- (min (mpfr-exp lo) (mpfr-exp hi) 0) (get-slack iter))])]
    [else
     (cond
       ; Can't both be inf, since:
       ;  - [inf, inf] not a valid interval
       ;  - [-inf, inf] crosses zero
       [(bfinfinite? lo) (mpfr-exp hi)]
       [(bfinfinite? hi) (mpfr-exp lo)]
       [else (min (mpfr-exp lo) (mpfr-exp hi))])]))

(define (logspan x)
  #;(define lo (ival-lo x))
  #;(define hi (ival-hi x))
  #;(if (or (bfzero? lo) (bfinfinite? lo) (bfzero? hi) (bfinfinite? hi))
        (get-slack)
        (+ (abs (- (mpfr-exp lo) (mpfr-exp hi))) 1))
  0)

; Function calculates an ampl factor per input for a certain output and inputs using condition formulas,
;   where an ampl is an additional precision that needs to be added to srcs evaluation so,
;   that the output will be fixed in its precision when evaluating again
; Additionaly, the function retures lower bound of ampl factor for the early exit mechanism
; Output: '( '(upper-ampl-bound lower-ampl-bound) ...) with len(srcs) number of elements
(define (get-bounds op z srcs)
  (case (object-name op)
    [(ival-mult)
     ; k = 1: logspan(y)
     ; k = 2: logspan(x)
     (define x (first srcs))
     (define y (second srcs))
     (list (cons (logspan y) 0) ; bounds per x
           (cons (logspan x) 0))] ; bounds per y

    [(ival-div)
     ; k = 1: logspan(y)
     ; k = 2: logspan(x) + 2 * logspan(y)
     (define x (first srcs))
     (define y (second srcs))
     (list (cons (logspan y) 0) ; bounds per x
           (cons (+ (logspan x) (* 2 (logspan y))) 0))] ; bounds per y

    [(ival-sqrt ival-cbrt)
     ; sqrt: logspan(x)/2 - 1
     ; cbrt: logspan(x)*2/3 - 1
     (define x (first srcs))
     (list (cons (quotient (logspan x) 2) 0))]

    [(ival-add ival-sub)
     ; k = 1: maxlog(x) - minlog(z)
     ; k = 2: maxlog(y) - minlog(z)
     (define x (first srcs))
     (define y (second srcs))

     (list (cons (- (maxlog x) (minlog z))
                 (- (minlog x #:no-slack #t) (maxlog z #:no-slack #t))) ; bounds per x
           (cons (- (maxlog y) (minlog z))
                 (- (minlog y #:no-slack #t) (maxlog z #:no-slack #t))))] ; bounds per y

    [(ival-pow)
     ; k = 1: maxlog(y) + logspan(x) + logspan(z)
     ; k = 2: maxlog(y) + max(|minlog(x)|,|maxlog(x)|) + logspan(z)
     (define x (first srcs))
     (define y (second srcs))

     ; when output crosses zero and x is negative - means that y was fractional and not fixed (specific of Rival)
     ; solution - add more slack for y to converge
     (define y-slack
       (if (and (crosses-zero? z) (bfnegative? (ival-lo x)))
           (get-slack)
           0))

     ; when output is (ival 0.bf 1.bf) - it means that x was close to 1 or 0 but not narrow enough
     (define x-slack
       (if (and (bfzero? (ival-lo z)) (bfinteger? (ival-hi z)))
           (get-slack)
           0))

     (list (cons (max (+ (maxlog y) (logspan x) (logspan z) x-slack) x-slack)
                 (minlog y #:no-slack #t)) ; bounds per x
           (cons (max (+ (maxlog y) (max (abs (maxlog x)) (abs (minlog x))) (logspan z) y-slack)
                      y-slack)
                 (minlog y #:no-slack #t)))] ; bounds per y

    [(ival-exp ival-exp2)
     ; maxlog(x) + logspan(z)
     (define x (car srcs))
     (list (cons (+ (maxlog x) (logspan z)) (minlog x #:no-slack #t)))]

    [(ival-tan)
     ; maxlog(x) + max(|minlog(z)|,|maxlog(z)|) + logspan(z) + 1
     (define x (first srcs))
     (list (cons (+ (maxlog x) (max (abs (maxlog z)) (abs (minlog z))) (logspan z) 1)
                 (+ (minlog x #:no-slack #t)
                    (min (abs (maxlog z #:no-slack #t)) (abs (minlog z #:no-slack #t))))))]

    [(ival-sin)
     ; maxlog(x) - minlog(z)
     (define x (first srcs))
     (list (cons (- (maxlog x) (minlog z)) (- (maxlog z #:no-slack #t))))]

    [(ival-cos)
     ; maxlog(x) - minlog(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (list (cons (+ (- (maxlog x) (minlog z)) (min (maxlog x) 0)) (- (maxlog z #:no-slack #t))))]

    [(ival-sinh)
     ; maxlog(x) + logspan(z) - min(minlog(x), 0)
     (define x (first srcs))
     (list (cons (- (+ (maxlog x) (logspan z)) (min (minlog x) 0)) (max 0 (minlog x #:no-slack #t))))]

    [(ival-cosh)
     ; maxlog(x) + logspan(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (list (cons (+ (maxlog x) (logspan z) (min (maxlog x) 0)) (max 0 (minlog x #:no-slack #t))))]

    [(ival-log ival-log2 ival-log10)
     ; log:   logspan(x) - minlog(z)
     ; log2:  logspan(x) - minlog(z) + 1
     ; log10: logspan(x) - minlog(z) - 1
     (define x (first srcs))
     (list (cons (+ (- (logspan x) (minlog z)) 1) (- (maxlog z #:no-slack #t))))]

    [(ival-asin)
     ; Γ[asin] = | x / (sqrt(1-x^2) * arcsin(x))|
     ; ampl[asin] = maxlog(x) - log[1-x^2]/2 - minlog(z)
     ;                          ^^^^^^^^^^^^
     ;                          unknown part

     (define x (first srcs))
     (list (if (>= (maxlog z) 1)
               (cons (get-slack)
                     (get-slack (prev-iter))) ; assumes that log[1-x^2]/2 is equal to slack
               (cons 1 0)))]

    [(ival-acos)
     ; Γ[acos] = |- x / (sqrt(1-x^2) * arccos(x))|
     ; ampl[acos] = maxlog(x) - log[1-x^2]/2 - minlog(z)
     ;                          ^^^^^^^^^^^^
     ;                          unknown part

     (define x (first srcs))
     (list (if (>= (maxlog x) 0)
               (cons (get-slack)
                     (get-slack (prev-iter))) ; assumes that log[1-x^2]/2 is equal to slack
               (cons 0 0)))]

    [(ival-atan)
     ; Γ[atan] = | x / ((1+x^2) * arctan(x))|
     ; ampl[atan] = logspan(x) - min(|minlog(x)|, |maxlog(x)|) - minlog(z)
     (define x (first srcs))
     (list (cons (- (logspan x) (min (abs (minlog x)) (abs (maxlog x))) (minlog z))
                 (- (- (max (abs (minlog x #:no-slack #t)) (abs (maxlog x #:no-slack #t))))
                    (maxlog z #:no-slack #t)
                    2)))]

    [(ival-fmod ival-remainder)
     ; x mod y = x - y*q, where q is rnd_down(x/y)
     ; k = 1: maxlog(x) - minlog(z)
     ; k = 2: ~ log[y * rnd_down(x/y)] - log[mod(x,y)] <= maxlog(x) - minlog(z)
     ;                            ^    ^
     ;                           conditions of uncertainty
     (define x (first srcs))
     (define y (second srcs))

     (define slack
       (if (crosses-zero? y)
           (get-slack) ; y crosses zero
           0))

     (list (cons (- (maxlog x) (minlog z)) 0) ; bounds per x
           (cons (max (+ (- (maxlog x) (minlog z)) slack) slack) 0))] ; bounds per y

    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; maxlog(x) - log[1+x] - minlog(z)
     ;            ^^^^^^^^^^
     ;            treated like a slack if x < 0
     (define x (first srcs))
     (define xhi (ival-hi x))
     (define xlo (ival-lo x))

     (list (if (or (equal? (mpfr-sign xlo) -1) (equal? (mpfr-sign xhi) -1))
               (cons (max (+ (- (maxlog x) (minlog z)) (get-slack)) (get-slack)) 0) ; if x in negative
               (cons (- (maxlog x) (minlog z)) 0)))]

    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1 + maxlog(x), 1 + maxlog(x) - minlog(z))
     (define x (first srcs))
     (list (cons (max (+ 1 (maxlog x)) (+ 1 (- (maxlog x) (minlog z)))) 0))]

    [(ival-atan2)
     ; maxlog(x) + maxlog(y) - 2*max(minlog(x), minlog(y)) - minlog(z)
     (define x (first srcs))
     (define y (second srcs))

     (make-list 2
                (cons (- (+ (maxlog x) (maxlog y)) (* 2 (max (minlog x) (minlog y))) (minlog z))
                      (- (+ (minlog x #:no-slack #t) (minlog y #:no-slack #t))
                         (* 2 (min (maxlog x #:no-slack #t) (maxlog y #:no-slack #t)))
                         (maxlog z #:no-slack #t))))]

    [(ival-tanh)
     ; logspan(z) + logspan(x)
     (define x (first srcs))
     (list (cons (+ (logspan z) (logspan x)) 0))]

    [(ival-atanh)
     ; log[Гarctanh] = maxlog(x) - log[(1-x^2)] - minlog(z) = 1 if x < 0.5, otherwise slack
     ;                               ^^^^^^^
     ;                           a possible uncertainty
     (define x (first srcs))
     (list (if (>= (maxlog x) 1)
               (cons (get-slack) (get-slack (prev-iter)))
               (cons 1 0)))]

    [(ival-acosh)
     ; log[Гacosh] = log[x / (sqrt(x-1) * sqrt(x+1) * acosh)] <= -minlog(z) + slack
     (define z-exp (minlog z))
     (list (if (< z-exp 2) ; when acosh(x) < 1
               (cons (max (- (get-slack) z-exp) (get-slack)) 0)
               (cons 0 0)))]

    [(ival-pow2)
     ; same as multiplication
     (define x (first srcs))
     (list (cons (+ (logspan x) 1) 0))]

    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb)
     (list (cons (get-slack) (get-slack (prev-iter))))]
    ; TODO
    [(ival-ceil ival-floor ival-rint ival-round ival-trunc)
     (list (cons (get-slack) (get-slack (prev-iter))))]

    [else (map (λ (_) (cons 0 0)) srcs)])) ; exponents for arguments

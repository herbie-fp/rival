#lang racket/base

(require racket/match
         racket/function
         racket/list)

(require "../mpfr.rkt"
         "../ops/all.rkt"
         "machine.rkt")

(provide get-bounds
         get-slack
         ival-info)

(define (get-slack [iter (*sampling-iteration*)])
  (match iter
    [0 0]
    [_ (* (expt 2 (- iter 1)) 512)]))

(define (crosses-zero? x)
  (not (equal? (mpfr-sign (ival-lo x)) (mpfr-sign (ival-hi x)))))

; We assume the interval x is valid. Critical not to take mpfr-exp of inf or 0
(define (maxlog x #:less-slack [less-slack #f])
  (define iter
    (if less-slack
        (- (*sampling-iteration*) 1)
        (*sampling-iteration*)))
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (define slack (get-slack iter))
  (cond
    [(and (bfinfinite? hi) (bfinfinite? lo)) slack] ; x = [-inf, inf]
    [(bfinfinite? hi) (+ (max (mpfr-exp lo) 0) slack)] ; x = [..., inf]
    [(bfinfinite? lo) (+ (max (mpfr-exp hi) 0) slack)] ; x = [-inf, ...]
    [else
     (+ (max (mpfr-exp lo) (mpfr-exp hi)) 1)])) ; x does not contain inf, safe with respect to 0.bf

(define (minlog x #:less-slack [less-slack #f])
  (define iter
    (if less-slack
        (- (*sampling-iteration*) 1)
        (*sampling-iteration*)))
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (define slack (get-slack iter))
  (cond
    [(and (bfzero? lo) (bfzero? hi)) (- slack)] ; x = [0.bf, 0.bf]
    [(bfzero? lo)
     (if (bfinfinite? hi)
         (- slack) ; x = [0.bf, +inf]
         (- (min (mpfr-exp hi) 0) slack))] ; x = [0.bf, ...]
    [(bfzero? hi)
     (if (bfinfinite? lo)
         (- slack) ; x = [-inf, 0.bf]
         (- (min (mpfr-exp lo) 0) slack))] ; x = [..., 0.bf]
    [(crosses-zero? x) ; x = [-..., +...]
     (cond
       [(and (bfinfinite? hi) (bfinfinite? lo)) (- slack)]
       [(bfinfinite? hi) (- (min (mpfr-exp lo) 0) slack)]
       [(bfinfinite? lo) (- (min (mpfr-exp hi) 0) slack)]
       [else (- (min (mpfr-exp lo) (mpfr-exp hi) 0) slack)])]
    [else
     (cond
       ; Can't both be inf, since:
       ;  - [inf, inf] not a valid interval
       ;  - [-inf, inf] crosses zero
       [(bfinfinite? lo) (mpfr-exp hi)]
       [(bfinfinite? hi) (mpfr-exp lo)]
       [else (min (mpfr-exp lo) (mpfr-exp hi))])]))

; Returns (list x (minlog x) (maxlog x) (logspan x) (crosses-zero? x))
(define (ival-info x)
  (define slack (get-slack))
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (match (and (boolean? lo) (boolean? hi))
    [#t (list #f #f #f #f #f)]
    [#f
     (define lo-exp (mpfr-exp lo))
     (define hi-exp (mpfr-exp hi))
     (define lo-sgn (mpfr-sign lo))
     (define hi-sgn (mpfr-sign hi))

     (define-values (zero-lo? inf-lo? zero-hi? inf-hi? crosses-0?)
       (values (bfzero? lo)
               (bfinfinite? lo)
               (bfzero? hi)
               (bfinfinite? hi)
               (not (equal? lo-sgn hi-sgn))))
     ; logspan
     (define lg
       (match (*bumps-activated*)
         [#t
          (if (or zero-lo? inf-lo? zero-hi? inf-hi?)
              slack
              (+ (abs (- lo-exp hi-exp)) 1))]
         [#f 0]))
     ; minlog+maxlog
     (define-values (mn mx)
       (cond
         ; x = [0.bf, 0.bf]
         [(and zero-lo? zero-hi?) (values (- slack) (- slack))]
         [zero-lo?
          (if inf-hi?
              (values (- slack) slack) ; x = [0.bf, +inf]
              (values (- (min hi-exp 0) slack) (+ hi-exp 1)))] ; x = [0.bf, +...]
         [zero-hi?
          (if inf-lo?
              (values (- slack) slack) ; x = [-inf, 0.bf]
              (values (- (min lo-exp 0) slack) (+ lo-exp 1)))] ; x = [-..., 0.bf]
         [crosses-0? ; x = [-..., +...]
          (cond
            [(and inf-hi? inf-lo?) (values (- slack) slack)] ; [-inf, +inf]
            [inf-hi? (values (- (min lo-exp 0) slack) (+ (max lo-exp 0) slack))] ; [-..., +inf]
            [inf-lo? (values (- (min hi-exp 0) slack) (+ (max hi-exp 0) slack))] ; [-inf, +...]
            [else
             (values (- (min lo-exp hi-exp 0) slack) (+ (max lo-exp hi-exp) 1))])] ; x = [-..., +...]
         [else
          (cond
            ; Can't both be inf, since:
            ;  - [inf, inf] not a valid interval
            ;  - [-inf, inf] crosses zero
            [inf-lo? (values hi-exp (+ (max hi-exp 0) slack))] ; [-inf, -...]
            [inf-hi? (values lo-exp (+ (max lo-exp 0) slack))] ; [+..., +inf]
            [else (values (min lo-exp hi-exp) (+ (max lo-exp hi-exp) 1))])]))
     ; output
     (list x mn mx lg crosses-0?)]))

(define (logspan x)
  (match (*bumps-activated*)
    [#t
     (define lo (ival-lo x))
     (define hi (ival-hi x))
     (if (or (bfzero? lo) (bfinfinite? lo) (bfzero? hi) (bfinfinite? hi))
         (get-slack)
         (+ (abs (- (mpfr-exp lo) (mpfr-exp hi))) 1))]
    [#f 0]))

; Function calculates an ampl factor per input for a certain output and inputs using condition formulas,
;   where an ampl is an additional precision that needs to be added to srcs evaluation so,
;   that the output will be fixed in its precision when evaluating again
; Additionaly, the function retures lower bound of ampl factor for the early exit mechanism
; Output: '( '(upper-ampl-bound lower-ampl-bound) ...) with len(srcs) number of elements
(define (get-bounds op output srcs)
  (case (object-name op)
    [(ival-mult ival-mult!)
     ; Γ[*]'x     = 1
     ; ↑ampl[*]'x = logspan(y)
     ; ↓ampl[*]'x = 0
     ; --------------
     ; Γ[*]'y     = 1
     ; ↑ampl[*]'y = logspan(x)
     ; ↓ampl[*]'y = 0
     (match-define (list _ _ _ lg-x _) (first srcs))
     (match-define (list _ _ _ lg-y _) (second srcs))
     (list (cons lg-x 0) ; bounds per x
           (cons lg-y 0))] ; bounds per y

    [(ival-div ival-div!)
     ; Γ[/]'x     = 1
     ; ↑ampl[/]'x = logspan(y)
     ; ↓ampl[/]'x = 0
     ; --------------
     ; Γ[/]'y     = 1
     ; ↑ampl[/]'y = logspan(x) + 2 * logspan(y)
     ; ↓ampl[/]'y = 0
     (match-define (list _ _ _ lg-x _) (first srcs))
     (match-define (list _ _ _ lg-y _) (second srcs))
     (list (cons lg-y 0) ; bounds per x
           (cons (+ lg-x (* 2 lg-y)) 0))] ; bounds per y

    [(ival-sqrt ival-cbrt)
     ; Γ[sqrt]'x     = 0.5
     ; ↑ampl[sqrt]'x = logspan(x)/2 - 1
     ; ↓ampl[sqrt]'x = 0
     ; --------------
     ; Γ[cbrt]'x     = 1/3
     ; ↑ampl[cbrt]'x = logspan(x)*2/3 - 1
     ; ↓ampl[cbrt]'x = 0
     (match-define (list _ _ _ lg-x _) (first srcs))
     (list (cons (quotient lg-x 2) 0))]

    [(ival-add ival-sub ival-add! ival-sub!)
     ; Γ[+ & -]'x     = |x/(x+y)| & |x/(x-y)|
     ; ↑ampl[+ & -]'x = maxlog(x) - minlog(z)
     ; ↓ampl[+ & -]'x = minlog(x) - maxlog(z)
     ; --------------
     ; Γ[+ & -]'y     = |y/(x+y)| & |-y/(x-y)|
     ; ↑ampl[+ & -]'y = maxlog(y) - minlog(z)
     ; ↓ampl[+ & -]'y = minlog(y) - maxlog(z)
     (match-define (list x _ mx-x _ _) (first srcs))
     (match-define (list y _ mx-y _ _) (second srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (- mx-x mn-z)
                     (- (minlog x #:less-slack #t) (maxlog z #:less-slack #t))) ; bounds per x
               (cons (- mx-y mn-z)
                     (- (minlog y #:less-slack #t) (maxlog z #:less-slack #t)))) ; bounds per y
         (list (cons (- mx-x mn-z) 0) ; bounds per x
               (cons (- mx-y mn-z) 0)))] ; bounds per y

    [(ival-pow)
     ; Γ[pow]'x     = |y|
     ; ↑ampl[pow]'x = maxlog(y) + logspan(x) + logspan(z)
     ; ↓ampl[pow]'x = minlog(y)
     ; --------------
     ; Γ[pow]'y     = |y*ln(x)|
     ; ↑ampl[pow]'y = maxlog(y) + max(|minlog(x)|,|maxlog(x)|) + logspan(z)
     ; ↓ampl[pow]'y = minlog(y)
     (match-define (list x mn-x mx-x lg-x _) (first srcs))
     (match-define (list y _ mx-y _ _) (second srcs))
     (match-define (list z _ _ lg-z cr-z) output)

     ; when output crosses zero and x is negative - means that y was fractional and not fixed (specific of Rival)
     ; solution - add more slack for y to converge
     (define y-slack
       (if (and cr-z (bfnegative? (ival-lo x)))
           (get-slack)
           0))

     ; when output is (ival 0.bf ...) - it means that x was close to 1 or 0 but not narrow enough
     (define x-slack
       (if (bfzero? (ival-lo z))
           (get-slack)
           0))

     (if (*lower-bound-early-stopping*)
         (list (cons (max (+ mx-y lg-x lg-z x-slack) x-slack)
                     (minlog y #:less-slack #t)) ; bounds per x
               (cons (max (+ mx-y (max (abs mx-x) (abs mn-x)) lg-z y-slack) y-slack)
                     (cond
                       [(zero? (min (abs mx-x) (abs mn-x))) 0]
                       [else (minlog y #:less-slack #t)]))) ; bounds per y
         (list (cons (max (+ mx-y lg-x lg-z x-slack) x-slack) 0) ; bounds per x
               (cons (max (+ mx-y (max (abs mx-x) (abs mn-x)) lg-z y-slack) y-slack)
                     0)))] ; bounds per y

    [(ival-exp ival-exp2)
     ; Γ[exp & exp2]'x     = |x| & |x*ln(2)|
     ; ↑ampl[exp & exp2]'x = maxlog(x) + logspan(z)
     ; ↓ampl[exp & exp2]'x = minlog(x)
     (match-define (list x _ mx-x _ _) (first srcs))
     (match-define (list _ _ _ lg-z _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (+ mx-x lg-z) (minlog x #:less-slack #t)))
         (list (cons (+ mx-x lg-z) 0)))]

    [(ival-tan)
     ; Γ[tan]'x     = |x / (cos(x) * sin(x))|
     ; ↑ampl[tan]'x = maxlog(x) + max(|minlog(z)|,|maxlog(z)|) + logspan(z) + 1
     ; ↓ampl[tan]'x = minlog(x) + min(|minlog(z)|,|maxlog(z)|) - 1
     (match-define (list x _ mx-x _ _) (first srcs))
     (match-define (list z mn-z mx-z lg-z _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (+ mx-x (max (abs mx-z) (abs mn-z)) lg-z 1)
                     (- (+ (minlog x #:less-slack #t)
                           (min (abs (maxlog z #:less-slack #t)) (abs (minlog z #:less-slack #t))))
                        1)))
         (list (cons (+ mx-x (max (abs mx-z) (abs mn-z)) lg-z 1) 0)))]

    [(ival-sin)
     ; Γ[sin]'x     = |x * cos(x) / sin(x)|
     ; ↑ampl[sin]'x = maxlog(x) - minlog(z)
     ; ↓ampl[sin]'x = | - maxlog(z) - 1, if maxlog(x) > 1
     ;                | 0 else
     (match-define (list _ _ mx-x _ _) (first srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (- mx-x mn-z)
                     (if (>= mx-x 1)
                         (- -1 (maxlog z #:less-slack #t))
                         0)))
         (list (cons (- mx-x mn-z) 0)))]

    [(ival-cos)
     ; Γ[cos]'x     = |x * sin(x) / cos(x)|
     ; ↑ampl[cos]'x = maxlog(x) - minlog(z) + min(maxlog(x), 0)
     ; ↓ampl[cos]'x = - maxlog(x) - 2
     (match-define (list _ _ mx-x _ _) (first srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (+ (- mx-x mn-z) (min mx-x 0)) (- (- 2) (maxlog z #:less-slack #t))))
         (list (cons (+ (- mx-x mn-z) (min mx-x 0)) 0)))]

    [(ival-sinh)
     ; Γ[sinh]'x     = |x * cosh(x) / sinh(x)|
     ; ↑ampl[sinh]'x = maxlog(x) + logspan(z) - min(minlog(x), 0)
     ; ↓ampl[sinh]'x = max(0, minlog(x))
     (match-define (list x mn-x mx-x _ _) (first srcs))
     (match-define (list _ _ _ lg-z _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (- (+ mx-x lg-z) (min mn-x 0)) 0 (max 0 (minlog x #:less-slack #t))))
         (list (cons (- (+ mx-x lg-z) (min mn-x 0)) 0)))]

    [(ival-cosh)
     ; Γ[cosh]'x     = |x * sinh(x) / cosh(x)|
     ; ↑ampl[cosh]'x = maxlog(x) + logspan(z) + min(maxlog(x), 0)
     ; ↓ampl[cosh]'x = max(0, minlog(x) - 1)
     (match-define (list x _ mx-x lg-x _) (first srcs))
     (match-define (list _ _ _ lg-z _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (+ mx-x lg-z (min mx-x 0)) (max 0 (- (minlog x #:less-slack #t) 1))))
         (list (cons (+ mx-x lg-z (min mx-x 0)) 0)))]

    [(ival-log ival-log2 ival-log10)
     ; Γ[log & log2 & log10]'x     = |1 / ln(x)| & |ln(2) / ln(x)| & |ln(10) / ln(x)|
     ; ↑ampl[log & log2 & log10]'x = logspan(x) - minlog(z) + 1
     ; ↓ampl[log & log2 & log10]'x = - maxlog(z)
     (match-define (list _ _ _ lg-x _) (first srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (+ (- lg-x mn-z) 1) (- (maxlog z #:less-slack #t))))
         (list (cons (+ (- lg-x mn-z) 1) 0)))]

    [(ival-asin)
     ; Γ[asin]'x     = |x / (sqrt(1-x^2) * arcsin(x))|
     ; ↑ampl[asin]'x = | slack, if maxlog(z) > 1
     ;                 | 1 else
     ; ↓ampl[asin]'x = 0
     (match-define (list _ _ mx-z _ _) output)

     (list (if (>= mx-z 1)
               (cons (get-slack) 0) ; assumes that log[1-x^2]/2 is equal to slack
               (cons 1 0)))]

    [(ival-acos)
     ; Γ[acos]'x     = |-x / (sqrt(1-x^2) * arccos(x))|
     ; ↑ampl[acos]'x = | slack, if maxlog(x) >= 0
     ;                 | 0 else
     ; ↓ampl[acos]'x = 0
     (match-define (list _ _ mx-x _ _) (first srcs))

     (list (if (>= mx-x 0)
               (cons (get-slack) 0) ; assumes that log[1-x^2]/2 is equal to slack
               (cons 0 0)))]

    [(ival-atan)
     ; Γ[atan]'x     = | x / ((1+x^2) * arctan(x))|
     ; ↑ampl[atan]'x = - min(|minlog(x)|, |maxlog(x)|) - minlog(z) + logspan(x)
     ; ↓ampl[atan]'x = - max(|minlog(x)|, |maxlog(x)|) - maxlog(z) - 2
     (match-define (list x mn-x mx-x lg-x _) (first srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (list (cons (- lg-x (min (abs mn-x) (abs mx-x)) mn-z)
                     (- (- (max (abs (minlog x #:less-slack #t)) (abs (maxlog x #:less-slack #t))))
                        (maxlog z #:less-slack #t)
                        2)))
         (list (cons (- lg-x (min (abs mn-x) (abs mx-x)) mn-z) 0)))]

    [(ival-fmod ival-remainder)
     ; ; x mod y = x - y*q, where q is rnd_down(x/y)
     ; Γ[mod]'x     ~ |x/mod(x,y)|
     ; ↑ampl[mod]'x = maxlog(x) - minlog(z)
     ; ↓ampl[mod]'x = minlog(x) - maxlog(z) or just 0
     ; --------------
     ; Γ[mod]'y     ` |y/mod(x,y)|
     ; ↑ampl[mod]'y = maxlog(y) - minlog(z)
     ; ↓ampl[mod]'y = minlog(y) - maxlog(z) or just 0
     (match-define (list _ _ mx-x _ _) (first srcs))
     (match-define (list _ _ _ _ cr-y) (second srcs))
     (match-define (list _ mn-z _ _ _) output)

     (define slack
       (if cr-y
           (get-slack)
           0))

     (list (cons (- mx-x mn-z) 0) ; bounds per x
           (cons (+ (- mx-x mn-z) slack) 0))] ; bounds per y

    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; Γ[log1p]'x     ~ |x * log1p(x) / (1+x)|
     ; ↑ampl[log1p]'x = | maxlog(x) - minlog(z) + slack, if x is negative
     ;                  | maxlog(x) - minlog(z), else
     ; ↓ampl[log1p]'x = 0
     (match-define (list x _ mx-x _ _) (first srcs))
     (match-define (list _ mn-z _ _ _) output)

     (list (if (equal? (mpfr-sign (ival-lo x)) -1)
               (cons (+ (- mx-x mn-z) (get-slack)) 0)
               (cons (- mx-x mn-z) 0)))]

    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; Γ[expm1]'x     = |x * e^x / expm1|
     ; ↑ampl[expm1]'x = max(1 + maxlog(x), 1 + maxlog(x) - minlog(z))
     ; ↓ampl[expm1]'x = 0
     (match-define (list _ _ mx-x _ _) (first srcs))
     (match-define (list _ mn-z _ _ _) output)

     (list (cons (max (+ 1 mx-x) (+ 1 (- mx-x mn-z))) 0))]

    [(ival-atan2)
     ; Γ[atan2]'x = Γ[atan2]'y = |xy / ((x^2 + y^2)*arctan(y/x))|
     ; ↑ampl[expm1]'x          = maxlog(x) + maxlog(y) - 2*min(minlog(x), minlog(y)) - minlog(z)
     ; ↓ampl[expm1]'x          = minlog(x) + minlog(y) - 2*max(maxlog(x), maxlog(y)) - maxlog(z)
     (match-define (list x mn-x mx-x _ _) (first srcs))
     (match-define (list y mn-y mx-y _ _) (second srcs))
     (match-define (list z mn-z _ _ _) output)

     (if (*lower-bound-early-stopping*)
         (make-list 2
                    (cons (- (+ mx-x mx-y) (* 2 (min mn-x mn-y)) mn-z)
                          (- (+ (minlog x #:less-slack #t) (minlog y #:less-slack #t))
                             (* 2 (max (maxlog x #:less-slack #t) (maxlog y #:less-slack #t)))
                             (maxlog z #:less-slack #t))))
         (make-list 2 (cons (- (+ mx-x mx-y) (* 2 (min mn-x mn-y)) mn-z) 0)))]

    [(ival-tanh)
     ; Γ[tanh]'x     = |x / (sinh(x) * cosh(x))|
     ; ↑ampl[tanh]'x = logspan(z) + logspan(x)
     ; ↓ampl[tanh]'x = 0
     (match-define (list _ _ _ lg-x _) (first srcs))
     (match-define (list _ _ _ lg-z _) output)

     (list (cons (+ lg-z lg-x) 0))]

    [(ival-atanh)
     ; Γ[atanh]'x     = |x / (log(1-x^2) * atanh(x))|
     ; ↑ampl[atanh]'x = | 1, if x < 0.5
     ;                  | slack
     ; ↓ampl[atanh]'x = 0
     (match-define (list _ _ mx-x _ _) (first srcs))
     (list (if (>= mx-x 1)
               (cons (get-slack) 0)
               (cons 1 0)))]

    [(ival-acosh)
     ; Γ[acosh]'x     = |x / (sqrt(x-1) * sqrt(x+1) * acosh)|
     ; ↑ampl[acosh]'x = | -minlog(z) + slack, if minlog(z) < 2
     ;                  | 0
     ; ↓ampl[acosh]'x = 0
     (match-define (list _ mn-z _ _ _) output)
     (list (if (< mn-z 2) ; when acosh(x) < 1
               (cons (- (get-slack) mn-z) 0)
               (cons 0 0)))]

    [(ival-pow2)
     ; Γ[acosh]'x = |2 x x* / x^2|
     ; ↑ampl[pow2]'x = logspan(x) + 1
     ; ↓ampl[pow2]'x = 0
     (match-define (list _ _ _ lg-x _) (first srcs))
     (list (cons (+ lg-x 1) 0))]

    ; TODO
    ; ↑ampl[...] = slack
    ; ↓ampl[...] = 0
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb) (list (cons (get-slack) 0))]

    ; TODO
    ; ↑ampl[...] = slack
    ; ↓ampl[...] = 0
    [(ival-ceil ival-floor ival-rint ival-round ival-trunc) (list (cons (get-slack) 0))]

    [else (map (λ (_) (cons 0 0)) srcs)])) ; exponents for arguments

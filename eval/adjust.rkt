#lang racket/base

(require racket/function
         racket/list
         racket/match)
(require "../mpfr.rkt"
         "../ops/all.rkt"
         "machine.rkt")
(provide backward-pass)

(define (backward-pass machine)
  ; Since Step 2 writes into *sampling-iteration* if the max prec was reached - save the iter number for step 3
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define slackvec (rival-machine-output-distance machine))
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define vrepeats (rival-machine-repeats machine))
  (define vprecs (rival-machine-precisions machine))
  (define vstart-precs (rival-machine-initial-precisions machine))
  (define current-iter (rival-machine-iteration machine))
  (define bumps (rival-machine-bumps machine))

  (define varc (vector-length args))
  (define vprecs-new (make-vector (vector-length ivec) 0)) ; new vprecs vector

  ; Step 1. Adding slack in case of a rounding boundary issue
  (for ([root-reg (in-vector rootvec)]
        [disc (in-vector discs)]
        [out-dr? (in-vector slackvec)]
        #:when (>= root-reg varc)
        #:when out-dr?)
    (vector-set! vprecs-new (- root-reg varc) (get-slack)))

  ; Step 1b. Checking if a operation should be computed again at all
  (define vuseful (make-vector (vector-length ivec) #f))
  (for ([root (in-vector rootvec)] #:when (>= root varc))
    (vector-set! vuseful (- root varc) #t))
  (for ([reg (in-vector vregs (- (vector-length vregs) 1) (- varc 1) -1)]
        [instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [i (in-range (- (vector-length ivec) 1) -1 -1)]
        [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)])
    (cond
      [(and (ival-lo-fixed? reg) (ival-hi-fixed? reg)) (vector-set! vuseful i #f)]
      [useful?
       (for ([arg (in-list (cdr instr))] #:when (>= arg varc))
         (vector-set! vuseful (- arg varc) #t))]))

  ; Step 2. Precision tuning
  (precision-tuning ivec vregs vprecs-new varc vstart-precs vuseful)

  ; Step 3. Repeating precisions check
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (define any-false? #f)
  (for ([instr (in-vector ivec)]
        [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
        [prec-new (in-vector vprecs-new)]
        [result-old (in-vector vregs varc)]
        [n (in-naturals)])
    (define repeat
      (and (<= prec-new prec-old)
           (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc)))) (cdr instr))))
    (set! any-false? (or any-false? (not repeat)))
    (vector-set! vrepeats n repeat))

  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new)

  ; Step 5. If precisions have not changed but the point didn't converge. A problem exists - add slack to every op
  (unless any-false?
    (set-rival-machine-bumps! machine (add1 bumps))
    (define slack (get-slack))
    (for ([prec (in-vector vprecs)] [n (in-range (vector-length vprecs))])
      (define prec* (min (*rival-max-precision*) (+ prec slack)))
      (when (equal? prec* (*rival-max-precision*))
        (*sampling-iteration* (*rival-max-iterations*)))
      (vector-set! vprecs n prec*))
    (vector-fill! vrepeats #f)))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-new[i] = min( *rival-max-precision* max( *base-tuning-precision* (+ intro vstart-precs[i])),
;   intro = get-ampls(parent)
(define (precision-tuning ivec vregs vprecs-new varc vstart-precs vuseful)
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)] ; reversed over ivec
        [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)]
        [n (in-range (- (vector-length vregs) 1) -1 -1)]
        #:when useful?) ; reversed over indices of vregs

    (define op (car instr)) ; current operation
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n)) ; output of the current instr

    (define intro (vector-ref vprecs-new (- n varc))) ; intro for the current instruction
    (define ampls (get-ampls op output srcs)) ; ampls for the tail instructions

    (define final-parent-precision
      (max (+ intro (vector-ref vstart-precs (- n varc))) (*base-tuning-precision*)))

    (when (>= final-parent-precision (*rival-max-precision*)) ; Early stopping
      (*sampling-iteration* (*rival-max-iterations*)))

    ; Final precision assignment
    (vector-set! vprecs-new (- n varc) (min final-parent-precision (*rival-max-precision*)))

    ; Intro and ampl propogation for each tail instruction
    (for ([x (in-list tail-registers)]
          [ampl (in-list ampls)]
          #:when (>= x varc)) ; when tail register is not a variable
      ; check whether this op already has a precision that is higher
      (when (> (+ intro ampl) (vector-ref vprecs-new (- x varc)))
        (vector-set! vprecs-new (- x varc) (+ intro ampl))))))

(define (crosses-zero? x)
  (not (equal? (mpfr-sign (ival-lo x)) (mpfr-sign (ival-hi x)))))

; We assume the interval x is valid. Critical not to take mpfr-exp of inf or 0,
; the results are platform-dependant
(define (maxlog x)
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (cond
    ; x = [-inf, inf]
    [(and (bfinfinite? hi) (bfinfinite? lo)) (get-slack)]
    [(bfinfinite? hi) (+ (max (mpfr-exp lo) 0) (get-slack))] ; x = [..., inf]
    [(bfinfinite? lo) (+ (max (mpfr-exp hi) 0) (get-slack))] ; x = [-inf, ...]
    [else
     (+ (max (mpfr-exp lo) (mpfr-exp hi)) 1)])) ; x does not contain inf, safe with respect to 0.bf

(define (minlog x)
  (define lo (ival-lo x))
  (define hi (ival-hi x))
  (cond
    ; x = [0.bf, ...]
    [(bfzero? lo) (if (bfinfinite? hi) (- (get-slack)) (- (min (mpfr-exp hi) 0) (get-slack)))]
    ; x = [..., 0.bf]
    [(bfzero? hi) (if (bfinfinite? lo) (- (get-slack)) (- (min (mpfr-exp lo) 0) (get-slack)))]
    [(crosses-zero? x) ; x = [-..., +...]
     (cond
       [(and (bfinfinite? hi) (bfinfinite? lo)) (- (get-slack))]
       [(bfinfinite? hi) (- (min (mpfr-exp lo) 0) (get-slack))]
       [(bfinfinite? lo) (- (min (mpfr-exp hi) 0) (get-slack))]
       [else (- (min (mpfr-exp lo) (mpfr-exp hi) 0) (get-slack))])]
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
(define (get-ampls op z srcs)
  (case (object-name op)
    [(ival-mult)
     ; k = 1: logspan(y)
     ; k = 2: logspan(x)
     (define x (first srcs))
     (define y (second srcs))
     (list (logspan y) ; exponent per x
           (logspan x))] ; exponent per y

    [(ival-div)
     ; k = 1: logspan(y)
     ; k = 2: logspan(x) + 2 * logspan(y)
     (define x (first srcs))
     (define y (second srcs))
     (list (logspan y) ; exponent per x
           (+ (logspan x) (* 2 (logspan y))))] ; exponent per y

    [(ival-sqrt ival-cbrt)
     ; sqrt: logspan(x)/2 - 1
     ; cbrt: logspan(x)*2/3 - 1
     (define x (first srcs))
     (list (quotient (logspan x) 2))]

    [(ival-add ival-sub)
     ; k = 1: maxlog(x) - minlog(z)
     ; k = 2: maxlog(y) - minlog(z)
     (define x (first srcs))
     (define y (second srcs))

     (list (- (maxlog x) (minlog z)) ; exponent per x
           (- (maxlog y) (minlog z)))] ; exponent per y

    [(ival-pow)
     ; k = 1: maxlog(y) + logspan(x) + logspan(z)
     ; k = 2: maxlog(y) + max(|minlog(x)|,|maxlog(x)|) + logspan(z)
     (define x (first srcs))
     (define y (second srcs))

     ; when output crosses zero and x is negative - means that y was fractional and not fixed (specific of Rival)
     ; solution - add more slack for y to converge
     (define slack (if (and (crosses-zero? z) (bfnegative? (ival-lo x))) (get-slack) 0))

     (list (+ (maxlog y) (logspan x) (logspan z)) ; exponent per x
           (+ (maxlog y) (max (abs (maxlog x)) (abs (minlog x))) (logspan z) slack))] ; exponent per y

    [(ival-exp ival-exp2)
     ; maxlog(x) + logspan(z)
     (define x (car srcs))
     (list (+ (maxlog x) (logspan z)))]

    [(ival-tan)
     ; maxlog(x) + max(|minlog(z)|,|maxlog(z)|) + logspan(z) + 1
     (define x (first srcs))
     (list (+ (maxlog x) (max (abs (maxlog z)) (abs (minlog z))) (logspan z) 1))]

    [(ival-sin)
     ; maxlog(x) - minlog(z)
     (define x (first srcs))
     (list (- (maxlog x) (minlog z)))]

    [(ival-cos)
     ; maxlog(x) - minlog(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (list (+ (- (maxlog x) (minlog z)) (min (maxlog x) 0)))]

    [(ival-sinh)
     ; maxlog(x) + logspan(z) - min(minlog(x), 0)
     (define x (first srcs))
     (list (- (+ (maxlog x) (logspan z)) (min (minlog x) 0)))]

    [(ival-cosh)
     ; maxlog(x) + logspan(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (list (+ (maxlog x) (logspan z) (min (maxlog x) 0)))]

    [(ival-log ival-log2 ival-log10)
     ; log:   logspan(x) - minlog(z)
     ; log2:  logspan(x) - minlog(z) + 1
     ; log10: logspan(x) - minlog(z) - 1
     (define x (first srcs))
     (list (+ (- (logspan x) (minlog z)) 1))]

    [(ival-asin)
     ; maxlog(x) - log[1-x^2]/2 - minlog(z)
     ;             ^^^^^^^^^^^^
     ;             condition of uncertainty
     (define x (first srcs))
     (define slack
       (if (>= (maxlog z) 2) ; Condition of uncertainty
           (get-slack) ; assumes that log[1-x^2]/2 is equal to slack
           0))

     (list (+ (- (maxlog x) (minlog z)) slack))]

    [(ival-acos)
     ; maxlog(x) - log[1-x^2]/2 - minlog(z)
     ;             ^^^^^^^^^^^^
     ;             condition of uncertainty
     (define x (first srcs))
     (define slack
       (if (>= (maxlog x) 1) ; Condition of uncertainty
           (get-slack) ; assumes that log[1-x^2]/2 is equal to slack
           0))

     (list (+ (- (maxlog x) (minlog z)) slack))]

    [(ival-atan)
     ; logspan(x) - min(|minlog(x)|, |maxlog(x)|) - minlog(z)
     (define x (first srcs))
     (list (- (logspan x) (min (abs (minlog x)) (abs (maxlog x))) (minlog z)))]

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

     (list (- (maxlog x) (minlog z)) ; exponent per x
           (+ (- (maxlog x) (minlog z)) slack))] ; exponent per y

    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; maxlog(x) - log[1+x] - minlog(z)
     ;            ^^^^^^^^^^
     ;            treated like a slack if x < 0
     (define x (first srcs))
     (define xhi (ival-hi x))
     (define xlo (ival-lo x))

     (define slack
       (if (or (equal? (mpfr-sign xlo) -1) (equal? (mpfr-sign xhi) -1))
           (get-slack) ; if x in negative
           0))

     (list (+ (- (maxlog x) (minlog z)) slack))]

    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1 + maxlog(x), 1 + maxlog(x) - minlog(z))
     (define x (first srcs))
     (list (max (+ 1 (maxlog x)) (+ 1 (- (maxlog x) (minlog z)))))]

    [(ival-atan2)
     ; maxlog(x) + maxlog(y) - 2*max(minlog(x), minlog(y)) - minlog(z)
     (define x (first srcs))
     (define y (second srcs))

     (make-list 2 (- (+ (maxlog x) (maxlog y)) (* 2 (max (minlog x) (minlog y))) (minlog z)))]

    [(ival-tanh)
     ; logspan(z) + logspan(x)
     (define x (first srcs))
     (list (+ (logspan z) (logspan x)))]

    [(ival-atanh)
     ; log[Гarctanh] = maxlog(x) - log[(1-x^2)] - minlog(z) = 1 if x < 0.5, otherwise slack
     ;                               ^^^^^^^
     ;                           a possible uncertainty
     (define x (first srcs))
     (list (if (>= (maxlog x) 1) (get-slack) 1))]

    [(ival-acosh)
     ; log[Гacosh] = log[x / (sqrt(x-1) * sqrt(x+1) * acosh)] <= -minlog(z) + slack
     (define z-exp (minlog z))
     (define slack
       (if (< z-exp 2) ; when acosh(x) < 1
           (get-slack)
           0))

     (list (- slack z-exp))]

    [(ival-pow2)
     ; same as multiplication
     (define x (first srcs))
     (list (+ (logspan x) 1))]

    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb) (list (get-slack))]
    ; TODO
    [(ival-ceil ival-floor ival-rint ival-round ival-trunc) (list (get-slack))]

    [else (map (const 0) srcs)])) ; exponents for arguments

(define (get-slack)
  (match (*sampling-iteration*)
    [1 512]
    [2 1024]
    [3 2048]
    [4 4096]
    [5 8192]))

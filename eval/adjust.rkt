#lang racket/base

(require racket/function racket/list racket/match)
(require (only-in math/private/bigfloat/mpfr bigfloat? mpfr-exp mpfr-sign bfnegative?))
(require "../ops.rkt" "machine.rkt")
(provide backward-pass)

(define (backward-pass machine)
  ; Since Step 2 writes into *sampling-iteration* if the max prec was reached - save the iter number for step 3
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define vrepeats (rival-machine-repeats machine))
  (define vprecs (rival-machine-precisions machine))
  (define vstart-precs (rival-machine-initial-precisions machine))
  (define current-iter (rival-machine-iteration machine))
  (define bumps (rival-machine-bumps machine))

  (define varc (vector-length args))
  (define rootlen (vector-length rootvec))
  (define vprecs-new (make-vector (vector-length ivec) 0))          ; new vprecs vector
  ; Step 1. Adding slack in case of a rounding boundary issue
  (for/vector #:length rootlen
              ([root-reg (in-vector rootvec)] [disc (in-vector discs)]
               #:when (>= root-reg varc))                           ; when root is not a variable
    (when (bigfloat? (ival-lo (vector-ref vregs root-reg)))         ; when root is a real op
      (define result (vector-ref vregs root-reg))
      (when
          ; 1 ulp apart means double rounding issue possible
          (= 1 ((discretization-distance disc)
                ((discretization-convert disc) (ival-lo result))
                ((discretization-convert disc) (ival-hi result))))
        (vector-set! vprecs-new (- root-reg varc) (get-slack)))))

  ; Step 2. Precision tuning
  (precision-tuning ivec vregs vprecs-new varc vstart-precs)

  ; Step 3. Repeating precisions check
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (define any-false? #f)
  (for ([instr (in-vector ivec)]
        [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
        [prec-new (in-vector vprecs-new)]
        [n (in-naturals)])
    (define repeat
      (and (<= prec-new prec-old)
           (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc))))
                   (cdr instr))))
    (set! any-false? (or any-false? (not repeat)))
    (vector-set! vrepeats n repeat))

  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new)

  ; Step 5. If precisions have not changed but the point didn't converge. A problem exists - add slack to every op
  (unless any-false?
    (set-rival-machine-bumps! machine (add1 bumps))
    (define slack (get-slack))
    (for ([prec (in-vector vprecs)]
          [n (in-range (vector-length vprecs))])
      (define prec* (min (*rival-max-precision*) (+ prec slack)))
      (when (equal? prec* (*rival-max-precision*)) (*sampling-iteration* (*rival-max-iterations*)))
      (vector-set! vprecs n prec*))))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-new[i] = min( *rival-max-precision* max( *base-tuning-precision* (+ ampls-from-above vstart-precs[i])),
;   ampls-from-above = get-ampls(parent)
(define (precision-tuning ivec vregs vprecs-new varc vstart-precs)
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]   ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)])           ; reversed over indices of vregs

    (define op (car instr))                                         ; current operation
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n))                            ; output of the current instr
    
    (define ampls-from-above (vector-ref vprecs-new (- n varc)))    ; vprecs-new is shifted by varc elements from vregs
    (define ampls (get-ampls op output srcs))

    (define final-parent-precision (max (+ ampls-from-above
                                           (vector-ref vstart-precs (- n varc)))
                                        (*base-tuning-precision*)))

    ; This case is weird. if we have a cancellation in fma -> ival-mult in fma should be in higher precision
    (when (equal? op ival-fma)
      (set! final-parent-precision (+ final-parent-precision (car ampls))))
    
    (when (>= final-parent-precision (*rival-max-precision*))         ; Early stopping
      (*sampling-iteration* (*rival-max-iterations*)))
    
    ; Final precision assignment
    (vector-set! vprecs-new (- n varc) (min final-parent-precision (*rival-max-precision*)))

    (for ([x (in-list tail-registers)]
          [ampl (in-list ampls)]
          #:when (>= x varc)) ; when tail register is not a variable
      ; check whether this op already has a precision that is higher
      (when (> (+ ampls-from-above ampl) (vector-ref vprecs-new (- x varc)))
        (vector-set! vprecs-new (- x varc) (+ ampls-from-above ampl))))))

(define (exp-is-inf? x)
  (equal? x -9223372036854775805))
(define (exp-is-0? x)
  (equal? x -9223372036854775807))
(define (exp-is-max? x)
  (equal? x 1073741823))
(define (exp-is-min? x)
  (equal? x -1073741823))

; We assume that NaNs can not be at this step in the interval
(define (maxlog x)
  (define lo-exp (mpfr-exp (ival-lo x)))
  (define hi-exp (mpfr-exp (ival-hi x)))
  (if (exp-is-inf? hi-exp)                            ; no upper bound is defined
      (get-slack)
       ; this check is crucial because we have (abs maxlog) in formulas
      (if (and (exp-is-0? lo-exp) (exp-is-min? hi-exp)) ; underflow
          (- (get-slack))          
          (+ (max lo-exp hi-exp) 1))))

(define (minlog x)
  (define lo-exp (mpfr-exp (ival-lo x)))
  (define hi-exp (mpfr-exp (ival-hi x)))
  (if (exp-is-0? lo-exp)                              ; no lower bound is defined
      (- (get-slack))
      ; this check is crucial because we have (abs minlog) in formulas
      (if (and (exp-is-max? lo-exp) (exp-is-inf? hi-exp)) ; overflow
          (get-slack)             
          (- (max lo-exp hi-exp) 1))))

(define (logspan x)
  (define lo-exp (mpfr-exp (ival-lo x)))
  (define hi-exp (mpfr-exp (ival-hi x)))
  (if (or (<= lo-exp -9223372036854775805)            ; if log2 of any endpoint is undefined (0 or inf)
          (<= hi-exp -9223372036854775805))           ; then logspan is undefined as well - use slack
      (get-slack)
      (+ (abs (- lo-exp hi-exp)) 1)))


; Function calculates an exponent per input for a certain output and inputs using condition formulas,
;   where an exponent is an additional precision that needs to be added to srcs evaluation so,
;   that the output will be fixed in its precision when evaluating again
(define (get-ampls op z srcs)
  (case (object-name  op)
    [(ival-mult)
     (define x (first srcs))
     (define y (second srcs))
     (list (logspan y)                                ; exponent per x
           (logspan x))]                              ; exponent per y
    
    [(ival-div)
     (define x (first srcs))
     (define y (second srcs))
     (list (logspan y)                                ; exponent per x
           (+ (logspan x) (* 2 (logspan y))))]        ; exponent per y

    [(ival-sqrt ival-cbrt)
     (define x (first srcs))
     (list (quotient (logspan x) 2))] ; which is [logspan(x)/2 - 1] or [logspan(x)*2/3 - 1]
    
    [(ival-add ival-sub)
     (define x (first srcs))
     (define y (second srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     (define slack (if (equal? (mpfr-sign zlo) (mpfr-sign zhi))
                       0
                       (get-slack)))
     
     (list (+ (- (maxlog x) (minlog z)) slack)   ; exponent per x
           (+ (- (maxlog y) (minlog z)) slack))] ; exponent per y 
    
    [(ival-pow)
     ; k = 1: maxlog(y) + logspan(x) + logspan(z)
     ; k = 2: maxlog(y) + |maxlog(x)| - 1 + logspan(z)
     (define x (first srcs))
     (define y (second srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     ; when output crosses zero and x is negative - means that y was fractional and not fixed
     ; solution - add more slack for y to converge
     (define slack (if (and (not (equal? (mpfr-sign zlo) (mpfr-sign zhi)))
                              (bfnegative? (ival-lo x)))
                         (get-slack)
                         0))
        
     (list (+ (maxlog y) (logspan x) (logspan z))     ; exponent per x
           (+ (maxlog y) (abs (maxlog x)) (logspan z) -1 slack))]  ; exponent per y
     
    [(ival-exp ival-exp2)
     ; maxlog(x) + logspan(z)
     (define x (car srcs))
     (list (+ (maxlog x) (logspan z)))]
    
    [(ival-tan)
     ; maxlog(x) + |maxlog(z)| + logspan(z) + 1
     (define x (first srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign zlo) (mpfr-sign zhi)))
                        (>= (maxlog x) 2))            ; x >= 1.bf, ideally x > pi.bf
                       (get-slack)                    ; tan is (-inf, +inf) or around zero (but x != 0)
                       0))
     
     (list (+ (maxlog x) (abs (maxlog z)) (logspan z) 1 slack))]

    [(ival-sin)
     ; maxlog(x) - minlog(z)
     (define x (first srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign zlo) (mpfr-sign zhi)))
                        (>= (maxlog x) 2))
                       (get-slack)
                       0))
     
     (list (+ (- (maxlog x) (minlog z)) slack))]

    [(ival-cos)
     ; maxlog(x) - minlog(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     (define slack (if (not (equal? (mpfr-sign zlo) (mpfr-sign zhi)))
                       (get-slack)
                       0))

     (list (+ (- (maxlog x) (minlog z)) (min (maxlog x) 0) slack))]


    [(ival-sinh)
     ; maxlog(x) + logspan(z) - min(minlog(x), 0)
     (define x (first srcs))
     (list (- (+ (maxlog x) (logspan z)) (min (minlog x) 0)))]

    [(ival-cosh)
     ; maxlog(x) + logspan(z) + min(maxlog(x), 0)
     (define x (first srcs))
     (list (+ (maxlog x) (logspan z) (min (minlog x) 0)))]
    
    [(ival-log ival-log2 ival-log10)
     ; logspan(x) - minlog(z)
     (define x (first srcs))
     (define zlo (ival-lo z))
     (define zhi (ival-hi z))
     
     (define slack (if (equal? (mpfr-sign zlo) (mpfr-sign zhi))
                       0
                       (get-slack)))                  ; output crosses 0 - uncertainty
     
     (list (+ (- (logspan x) (minlog z)) slack))]

    ; asin and acos has not been changed
    [(ival-asin ival-acos)
     ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)] + 1
     ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)] + 1
     ;                       ^^^^^^^^^^^^
     ;                       condition of uncertainty
     (define x (first srcs))
     (define x-exp (maxlog x))
     (define out-exp (minlog z))
     
     (define slack (if (>= x-exp 1)                   ; Condition of uncertainty when argument > sqrt(3)/2
                       (get-slack)                    ; assumes that log[1-x^2]/2 is equal to slack
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]
    
    [(ival-atan)
     ; logspan(x) - |minlog(x)| - minlog(z)
     (define x (first srcs))
     
     (list (- (logspan x) (abs (minlog x)) (minlog z)))]


    ; has not been changed
    [(ival-fmod ival-remainder)
     ; x mod y = x - y*q, where q is rnd_down(x/y)
     ; log[Гmod]'x ~ log[x]                 - log[mod(x,y)] + 1
     ; log[Гmod]'y ~ log[y * rnd_down(x/y)] - log[mod(x,y)] + 1 <= log[x] - log[mod(x,y)] + 1
     ;                                 ^    ^
     ;                                 conditions of uncertainty
     (define x (first srcs))
     (define y (second srcs))
     (define ylo (ival-lo y))
     (define yhi (ival-hi y))
     (define outlo (ival-lo z))
     (define outhi (ival-hi z))
     
     (define x-exp (maxlog x))
     (define out-exp (minlog z))
     
     (define x-slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                         0                
                         (get-slack)))                ; output crosses 0
     
     (define y-slack (if (equal? (mpfr-sign ylo) (mpfr-sign yhi))
                         x-slack                
                         (+ x-slack (get-slack))))    ; y crosses zero
     
     (list (+ (- x-exp out-exp) 1 x-slack)            ; exponent per x
           (+ (- x-exp out-exp) 1 y-slack))]          ; exponent per y


    ; has not been changed
    [(ival-fma)
     ; log[Гfma] = log[ max(x*y, -z) / fma(x,y,z)] ~ max(log[x] + log[y], log[z]) - log[fma(x,y,z)] + 1
     ;                               ^^^^^^^^^^^^
     ;                               possible uncertainty
     (define x (first srcs))
     (define y (second srcs))
     (define z (third srcs))
     (define outlo (ival-lo z))
     (define outhi (ival-hi z))
     
     (define x-exp (maxlog x))
     (define y-exp (maxlog y))
     (define z-exp (maxlog z))
     (define out-exp (minlog z))
     
     (define slack (if (equal? (mpfr-sign outhi) (mpfr-sign outlo))
                                0
                                (get-slack)))         ; cancellation when output crosses 0
     
     (define lhs-exp (max (+ x-exp y-exp)             ; max(log[x] + log[y], log[z])
                          z-exp))
     (make-list 3 (+ (- lhs-exp out-exp) 1 slack))]   ; exponents per arguments

    ; has not been changed
    [(ival-hypot)
     ; hypot = sqrt(x^2+y^2)
     ; log[Гhypot] = log[ (2 * max(x,y) / hypot(x,y))^2 ] = 2 * (1 + log[max(x,y)] - log[hypot]) + 1
     ;                                  ^
     ;                                  a possible division by zero, catched by log2-approx's slack
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (maxlog x))
     (define y-exp (maxlog y))
     (define out-exp (minlog z))
     
     (make-list 2 (+ (* 2 (- (+ 1 (max x-exp y-exp)) out-exp)) 1))]

    ; has not been changed
    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; log[Гlog1p] = log[x] - log[1+x] - log[log1p] + 1
     ;                      ^^^^^^^^^^
     ;                      treated like a slack if x < 0
     (define x (first srcs))
     (define xhi (ival-hi x))
     (define xlo (ival-lo x))
     
     (define x-exp (maxlog x))
     (define out-exp (minlog z))
     
     (define slack (if (or (equal? (mpfr-sign xlo) -1)
                           (equal? (mpfr-sign xhi) -1))
                       (get-slack)                    ; if x in negative
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]

    ; has not been changed
    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1+log[x], 1+log[x/expm1] +1)
     ;                                                                    ^^ division accounting
     (define x (first srcs))
     (define x-exp (maxlog x))
     (define out-exp (minlog z))
     
     (list (max (+ 1 x-exp) (+ 2 (- x-exp out-exp))))]

    
    [(ival-atan2)
     ; maxlog(x) + maxlog(y) - 2*max(minlog(x), minlog(y)) - minlog(z)
     (define x (first srcs))
     (define y (second srcs))
     
     (make-list 2 (- (+ (maxlog x) (maxlog y)) (* 2 (max (minlog x) (minlog y))) (minlog z)))]
    
    [(ival-tanh)
     ; logspan(z) + logspan(x)
     (define x (first srcs))
     (list (+ (logspan z) (logspan x)))]

    ; has not been changed
    [(ival-atanh)
     ; log[Гarctanh] = log[x / ((1-x^2) * atanh)] = 1 if x < 0.5, otherwise slack
     ;                          ^^^^^^^
     ;                          a possible uncertainty
     (define x (first srcs))
     (define x-exp (maxlog x))
     
     (list (if (>= x-exp 1)
               (get-slack)
               1))]

    ; has not been chaned
    [(ival-acosh)
     ; log[Гacosh] = log[x / (sqrt(x-1) * sqrt(x+1) * acosh)] <= -log[acosh] + slack
     (define out-exp (minlog z))
     (define slack (if (< out-exp 2)                  ; when acosh(x) < 1
                       (get-slack)
                       0))
     
     (list (+ (- out-exp) slack))]
    
    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb)
     (list (get-slack))]
    [else (map (const 0) srcs)]))        ; exponents for argumetns

(define (get-slack)
  (match (*sampling-iteration*)
    [0 256]
    [1 512]
    [2 1024]
    [3 2048]
    [4 4096]
    [5 8192]))

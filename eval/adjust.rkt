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
               #:when (>= root-reg varc))          ; when root is not a variable
    (when (bigfloat? (ival-lo (vector-ref vregs root-reg)))         ; when root is a real op
      (define result (vector-ref vregs root-reg))
      (when
          ; 1 ulp apart means double rounding issue possible
          (= 1 ((discretization-distance disc)
                ((discretization-convert disc) (ival-lo result))
                ((discretization-convert disc) (ival-hi result))))
        (vector-set! vprecs-new (- root-reg varc) (get-slack)))))

  ; Step 2. Exponents calculation
  (exponents-propogation ivec vregs vprecs-new varc vstart-precs)

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
    (set-rival-machine-bumps! (add1 bumps))
    (define slack (get-slack))
    (for ([prec (in-vector vprecs)]
          [n (in-range (vector-length vprecs))])
      (define prec* (min (*rival-max-precision*) (+ prec slack)))
      (when (equal? prec* (*rival-max-precision*)) (*sampling-iteration* (*rival-max-iterations*)))
      (vector-set! vprecs n prec*))))

; This function goes through ivec and vregs and calculates (+ exponents base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-new[i] = min( *rival-max-precision* max( *base-tuning-precision* (+ exponents-from-above vstart-precs[i])),
;   exponents-from-above = get-exponent(parent)
(define (exponents-propogation ivec vregs vprecs-new varc vstart-precs)
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]   ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)])           ; reversed over indices of vregs

    (define op (car instr))                                         ; current operation
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n))                            ; output of the current instr
    
    (define exps-from-above (vector-ref vprecs-new (- n varc)))     ; vprecs-new is shifted by varc elements from vregs
    (define new-exponents (get-exponent op output srcs))

    (define final-parent-precision (max (+ exps-from-above
                                           (vector-ref vstart-precs (- n varc)))
                                        (*base-tuning-precision*)))

    ; This case is weird. if we have a cancellation in fma -> ival-mult in fma should be in higher precision
    (when (equal? op ival-fma)
      (set! final-parent-precision (+ final-parent-precision (car new-exponents))))
    
    (when (>= final-parent-precision (*rival-max-precision*))         ; Early stopping
      (*sampling-iteration* (*rival-max-iterations*)))
    (vector-set! vprecs-new (- n varc) (min final-parent-precision (*rival-max-precision*)))

    (for ([x (in-list tail-registers)]
          [new-exp (in-list new-exponents)]
          #:when (>= x varc)) ; when tail register is not a variable
      ; check whether this op already has a precision that is higher
      (when (> (+ exps-from-above new-exp) (vector-ref vprecs-new (- x varc)))
        (vector-set! vprecs-new (- x varc) (+ exps-from-above new-exp))))))

(define (ival-max-log2-approx x)
  (max (log2-approx (ival-hi x)) (log2-approx (ival-lo x))))
(define (ival-min-log2-approx x)
  (min (log2-approx (ival-hi x)) (log2-approx (ival-lo x))))

; Function calculates an exponent per input for a certain output and inputs using condition formulas,
;   where an exponent is an additional precision that needs to be added to srcs evaluation so,
;   that the output will be fixed in its precision when evaluating again
(define (get-exponent op output srcs)
  (case (object-name  op)
    [(ival-mult ival-div ival-sqrt ival-cbrt)
     ; log[Г*]'x = log[Г*]'y = log[Г/]'x = log[Г/]'y = 1
     ; log[Гsqrt] = 0.5
     ; log[Гcbrt] = 0.3
     (map (const 0) srcs)]                     ; assume that *ampl-bits* already introduces this 1 bit
    
    [(ival-add ival-sub)
     ; log[Г+]'x = log[x] - log[x + y] + 1 (1 for mantissa approximation when dividing)
     ; log[Г+]'y = log[y] - log[x + y] + 1
     ; log[Г-]'x = log[x] - log[x - y] + 1
     ; log[Г-]'y = log[y] - log[x - y] + 1
     ;                    ^^^^^^^^^^^^
     ;                    possible cancellation
     (define x (first srcs))
     (define y (second srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                       0
                       (get-slack)))
     
     (list (+ (- x-exp out-exp) slack 1)              ; exponent per x
           (+ (- y-exp out-exp) slack 1))]            ; exponent per y 
    
    [(ival-pow)
     ; log[Гpow]'x = log[y]
     ; log[Гpow]'y = log[y] + log[log(x)] <= log[y] + |log[x]|

     (define x (first srcs))
     (define y (second srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     
     ; when output crosses zero and x is negative - means that y was fractional and not fixed
     ; solution - add more slack for y to converge
     (define y-slack (if (and (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                              (bfnegative? (ival-lo x)))
                         (get-slack)
                         0))

     #;(define xlo-exp (mpfr-exp (ival-lo x)))
     #;(define xhi-exp (mpfr-exp (ival-hi x)))
     #;(define x-slack ; it is likely to be a handling case from IEEE, x is not close enough
       (if (or (equal? xlo-exp -9223372036854775807)
               (equal? xhi-exp -9223372036854775807)        ; if interval contains 0
               (and (equal? 1 xlo-exp) (equal? 1 xhi-exp))) ; if interval possibly contains 1
           (get-slack)
           0))
        
     (list (+ y-exp #;x-slack)                        ; exponent per x
           (+ y-exp (abs x-exp) y-slack))]            ; exponent per y
     
    [(ival-exp ival-exp2)
     ; log[Гexp] = log[x]
     (define x (car srcs))
     (define x-exp (ival-max-log2-approx x))
     (list x-exp)]
    
    [(ival-tan)
     ; log[Гtan] = log[x] - log[sin(x)*cos(x)] <= log[x] + |log[tan(x)]| + 1
     ;                                                      ^^^^^^^^^^^
     ;                                                      possible uncertainty
     (define x (first srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp-abs (max (abs (log2-approx outlo))
                              (abs (log2-approx outhi))))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                        (>= x-exp 2))                 ; x >= 1.bf [log2-approx(1.bf) = 2], ideally x > pi.bf
                       (get-slack)                    ; tan is (-inf, +inf) or around zero (but x != 0)
                       0))
     
     (list (+ x-exp out-exp-abs 1 slack))]
    
    [(ival-sin ival-cos ival-sinh ival-cosh)
     ; log[Гcos] = log[x] + log[sin(x)] - log[cos(x)] <= log[x] - log[cos(x)] + 1
     ; log[Гsin] = log[x] + log[cos(x)] - log[sin(x)] <= log[x] - log[sin(x)] + 1
     ;                                                          ^^^^^^^^^^^^^
     ;                                                          a possible uncertainty
     (define x (first srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                        (>= x-exp 2))                 ; if x >= 1 [log2-approx(1.bf) = 2], when x=0 - no slack needed
                       (get-slack)                    ; Condition of uncertainty
                       0))
     
     (list (+ (- x-exp out-exp) slack 1))]
    
    [(ival-log ival-log2 ival-log10)
     ; log[Гlog]   = log[1/logx] = -log[log(x)]
     ; log[Гlog2]  = log[1/(log2(x) * ln(2))] <= -log[log2(x)] + 1    < main formula
     ; log[Гlog10] = log[1/(log10(x) * ln(10))] <= -log[log10(x)] - 1
     ; log[Гlog10] < log[Гlog] < log[Гlog2]
     
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     (define out-exp (- (ival-min-log2-approx output))) ; -log[log10(x)]
     
     (define slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                       0
                       (get-slack)))                  ; output crosses 0 - uncertainty
     
     (list (+ out-exp 1 slack))]
    
    [(ival-asin ival-acos)
     ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)] + 1
     ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)] + 1
     ;                       ^^^^^^^^^^^^
     ;                       condition of uncertainty
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (>= x-exp 1)                   ; Condition of uncertainty when argument > sqrt(3)/2
                       (get-slack)                    ; assumes that log[1-x^2]/2 is equal to slack
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]
    
    [(ival-atan)
     ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)] <= -|log[x]| - log[atan(x)] + 1 <= 0
     (define x (first srcs))
     
     (define x-exp-abs (max (abs (log2-approx (ival-hi x)))
                            (abs (log2-approx (ival-lo x)))))
     (define out-exp (ival-min-log2-approx output))
     
     (list (+ (- (- x-exp-abs) out-exp) 1))]
    
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
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define x-slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                         0                
                         (get-slack)))                ; output crosses 0
     
     (define y-slack (if (equal? (mpfr-sign ylo) (mpfr-sign yhi))
                         x-slack                
                         (+ x-slack (get-slack))))    ; y crosses zero
     
     (list (+ (- x-exp out-exp) 1 x-slack)            ; exponent per x
           (+ (- x-exp out-exp) 1 y-slack))]          ; exponent per y
    
    [(ival-fma)
     ; log[Гfma] = log[ max(x*y, -z) / fma(x,y,z)] ~ max(log[x] + log[y], log[z]) - log[fma(x,y,z)] + 1
     ;                               ^^^^^^^^^^^^
     ;                               possible uncertainty
     (define x (first srcs))
     (define y (second srcs))
     (define z (third srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define z-exp (ival-max-log2-approx z))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (equal? (mpfr-sign outhi) (mpfr-sign outlo))
                                0
                                (get-slack)))         ; cancellation when output crosses 0
     
     (define lhs-exp (max (+ x-exp y-exp)             ; max(log[x] + log[y], log[z])
                          z-exp))
     (make-list 3 (+ (- lhs-exp out-exp) 1 slack))]   ; exponents per arguments
    
    [(ival-hypot)
     ; hypot = sqrt(x^2+y^2)
     ; log[Гhypot] = log[ (2 * max(x,y) / hypot(x,y))^2 ] = 2 * (1 + log[max(x,y)] - log[hypot]) + 1
     ;                                  ^
     ;                                  a possible division by zero, catched by log2-approx's slack
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (make-list 2 (+ (* 2 (- (+ 1 (max x-exp y-exp)) out-exp)) 1))]
    
    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; log[Гlog1p] = log[x] - log[1+x] - log[log1p] + 1
     ;                      ^^^^^^^^^^
     ;                      treated like a slack if x < 0
     (define x (first srcs))
     (define xhi (ival-hi x))
     (define xlo (ival-lo x))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (or (equal? (mpfr-sign xlo) -1)
                           (equal? (mpfr-sign xhi) -1))
                       (get-slack)                    ; if x in negative
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]
    
    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1+log[x], 1+log[x/expm1] +1)
     ;                                                                    ^^ division accounting
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (list (max (+ 1 x-exp) (+ 2 (- x-exp out-exp))))]
    
    [(ival-atan2)
     ; log[Гatan2]'x = log[Гatan2]'y = log[xy / ((x^2+y^2)*atan2)] <= log[x] + log[y] - 2*max[logx, logy] - log[atan2] + 1
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (make-list 2 (+ (- (+ x-exp y-exp) (* 2 (max x-exp y-exp)) out-exp) 1))]
    
    ; Currently has a poor implementation
    [(ival-tanh)
     ; log[Гtanh] = log[x / (sinh(x) * cosh(x))] <= -log[x] + log[tanh]. Never greater than 0
     (define x (first srcs))
     (define x-exp (ival-min-log2-approx x))
     (define out-exp (ival-max-log2-approx output))
     
     (list (+ (- x-exp) out-exp))]
    
    [(ival-atanh)
     ; log[Гarctanh] = log[x / ((1-x^2) * atanh)] = 1 if x < 0.5, otherwise slack
     ;                          ^^^^^^^
     ;                          a possible uncertainty
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     
     (list (if (>= x-exp 1)
               (get-slack)
               1))]
    
    [(ival-acosh)
     ; log[Гacosh] = log[x / (sqrt(x-1) * sqrt(x+1) * acosh)] <= -log[acosh] + slack
     (define out-exp (ival-min-log2-approx output))
     (define slack (if (< out-exp 2)                  ; when acosh(x) < 1
                       (get-slack)
                       0))
     
     (list (+ (- out-exp) slack))]
    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb)
     (list (get-slack))]
    [else (map (const 0) srcs)]))        ; exponents for argumetns

(define (log2-approx x)
  (define exp (mpfr-exp x))
  (if (or (equal? exp -9223372036854775807) (equal? exp -1073741823))
      (- (get-slack))  ; 0.bf/min.bf
      (if (<= 1073741823 (abs exp))
          (get-slack)  ; max.bf/inf.bf/nan.bf
          (+ exp 1)))) ; +1 because mantissa is not considered

(define (get-slack)
  (match (*sampling-iteration*)
    [0 256]
    [1 512]
    [2 1024]
    [3 2048]
    [4 4096]
    [5 8192]))

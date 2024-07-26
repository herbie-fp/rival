#lang racket

(require racket/match
         (only-in "../mpfr.rkt" bfprev bf bfsinu bfcosu bftanu bf-rounding-mode bf=?)
         racket/flonum)
(require "../ops/all.rkt"
         "machine.rkt")
(provide rival-compile
         *rival-use-shorthands*
         *rival-name-constants*
         fn->ival-fn exprs->batch) ; for baseline

(define *rival-use-shorthands* (make-parameter #t))
(define *rival-name-constants* (make-parameter #f))


(define (fn->ival-fn node)
  (match node
    [(? number?)
     (if (ival-point? (real->ival node))
         (list (ival-const node))
         (list (ival-rational node)))]

    [(list 'PI)     (list ival-pi)]
    [(list 'E)      (list ival-e)]
    [(list 'INFINITY) (list ival-infinity)]
    [(list 'NAN)    (list ival-nan)]
    [(list 'TRUE)   (list ival-true)]
    [(list 'FALSE)  (list ival-false)]

    [(list 'if c y f) (list ival-if c y f)]

    [(list 'neg x)   (list ival-neg x)]
    [(list 'acos x)  (list ival-acos x)]
    [(list 'acosh x) (list ival-acosh x)]
    [(list 'asin x)  (list ival-asin x)]
    [(list 'asinh x) (list ival-asinh x)]
    [(list 'atan x)  (list ival-atan x)]
    [(list 'atanh x) (list ival-atanh x)]
    [(list 'cbrt x)  (list ival-cbrt x)]
    [(list 'ceil x)  (list ival-ceil x)]
    [(list 'cos x)   (list ival-cos x)]
    [(list 'cosh x)  (list ival-cosh x)]
    [(list 'erf x)   (list ival-erf x)]
    [(list 'erfc x)  (list ival-erfc x)]
    [(list 'exp x)   (list ival-exp x)]
    [(list 'exp2 x)  (list ival-exp2 x)]
    [(list 'expm1 x) (list ival-expm1 x)]
    [(list 'fabs x)  (list ival-fabs x)]
    [(list 'floor x) (list ival-floor x)]
    [(list 'lgamma x) (list ival-lgamma x)]
    [(list 'log x)   (list ival-log x)]
    [(list 'log10 x) (list ival-log10 x)]
    [(list 'log1p x) (list ival-log1p x)]
    [(list 'log2 x)  (list ival-log2 x)]
    [(list 'logb x)  (list ival-logb x)]
    [(list 'rint x)  (list ival-rint x)]
    [(list 'round x) (list ival-round x)]
    [(list 'sin x)   (list ival-sin x)]
    [(list 'sinh x)  (list ival-sinh x)]
    [(list 'sqrt x)  (list ival-sqrt x)]
    [(list 'tan x)   (list ival-tan x)]
    [(list 'tanh x)  (list ival-tanh x)]
    [(list 'tgamma x) (list ival-tgamma x)]
    [(list 'trunc x) (list ival-trunc x)]

    [(list '+ x y)     (list ival-add x y)]
    [(list '- x y)     (list ival-sub x y)]
    [(list '* x y)     (list ival-mult x y)]
    [(list '/ x y)     (list ival-div x y)]
    [(list 'atan2 x y) (list ival-atan2 x y)]
    [(list 'copysign x y) (list ival-copysign x y)]
    [(list 'hypot x y) (list ival-hypot x y)]
    [(list 'fdim x y)  (list ival-fdim x y)]
    [(list 'fmax x y)  (list ival-fmax x y)]
    [(list 'fmin x y)  (list ival-fmin x y)]
    [(list 'fmod x y)  (list ival-fmod x y)]
    [(list 'pow x y)   (list ival-pow x y)]
    [(list 'remainder x y) (list ival-remainder x y)]

    [(list 'pow2 x) (list ival-pow2 x)]

    [(list '== x y) (list ival-== x y)]
    [(list '!= x y) (list ival-!= x y)]
    [(list '<= x y) (list ival-<= x y)]
    [(list '>= x y) (list ival->= x y)]
    [(list '< x y)  (list ival-< x y)]
    [(list '> x y)  (list ival-> x y)]

    [(list 'not x)   (list ival-not x)]
    [(list 'and x y) (list ival-and x y)]
    [(list 'or x y)  (list ival-or x y)]

    [(list 'cast x)  (list values x)]

    [(list 'assert x)  (list ival-assert x)]
    [(list 'error x)  (list ival-error? x)]

    [(list op args ...)
     (error 'compile-specs "Unknown operator ~a" op)]))


(define (optimize expr)
  (match (and (*rival-use-shorthands*) expr)

    ; Syntax quirks
    [`PI '(PI)]
    [`E '(E)]
    [`(- ,x) `(neg ,x)]

    ; Special numeric functions
    [`(fma ,x ,y ,z) `(+ (* ,x ,y) ,z)]
    [`(- (exp ,x) 1) `(expm1 ,x)]
    [`(- 1 (exp ,x)) `(neg (expm1 ,x))]
    [`(log (+ 1 ,x)) `(log1p ,x)]
    [`(log (+ ,x 1)) `(log1p ,x)]
    [`(sqrt (+ (* ,x ,x) (* ,y ,y))) `(hypot ,x ,y)]
    [`(sqrt (+ (* ,x ,x) 1)) `(hypot ,x 1)]
    [`(sqrt (+ 1 (* ,x ,x))) `(hypot 1 ,x)]

    ; Special case powers
    [`(pow ,arg 2) `(pow2 ,arg)]
    [`(pow ,arg 1/3) `(cbrt ,arg)]
    [`(pow ,arg 1/2) `(sqrt ,arg)]

    ; Special trigonometric functions
    [`(cos (* ,(or 'PI '(PI)) (/ ,x ,(? (conjoin fixnum? positive?) n))))
     #:when bfcosu
     `((cosu ,(* 2 n)) ,x)]
    [`(cos (* (/ ,x ,(? (conjoin fixnum? positive?) n)) ,(or 'PI '(PI))))
     #:when bfcosu
     `((cosu ,(* 2 n)) ,x)]
    [`(cos (* ,(or 'PI '(PI)) ,x))
     #:when bfcosu
     `((cosu 2) ,x)]
    [`(cos (* ,x ,(or 'PI '(PI))))
     #:when bfcosu
     `((cosu 2) ,x)]
    [`(cos (* (* 2 ,(or 'PI '(PI))) ,x))
     #:when bfcosu
     `((cosu 1) ,x)]
    [`(cos (* ,x (* 2 ,(or 'PI '(PI)))))
     #:when bfcosu
     `((cosu 1) ,x)]
    [`(sin (* ,(or 'PI '(PI)) (/ ,x ,(? (conjoin fixnum? positive?) n))))
     #:when bfsinu
     `((sinu ,(* 2 n)) ,x)]
    [`(sin (* (/ ,x ,(? (conjoin fixnum? positive?) n)) ,(or 'PI '(PI))))
     #:when bfsinu
     `((sinu ,(* 2 n)) ,x)]
    [`(sin (* ,(or 'PI '(PI)) ,x))
     #:when bfsinu
     `((sinu 2) ,x)]
    [`(sin (* ,x ,(or 'PI '(PI))))
     #:when bfsinu
     `((sinu 2) ,x)]
    [`(sin (* (* 2 ,(or 'PI '(PI))) ,x))
     #:when bfsinu
     `((sinu 1) ,x)]
    [`(sin (* ,x (* 2 ,(or 'PI '(PI)))))
     #:when bfsinu
     `((sinu 1) ,x)]
    [`(tan (* ,(or 'PI '(PI)) (/ ,x ,(? (conjoin fixnum? positive?) n))))
     #:when bftanu
     `((tanu ,(* 2 n)) ,x)]
    [`(tan (* (/ ,x ,(? (conjoin fixnum? positive?) n)) ,(or 'PI '(PI))))
     #:when bftanu
     `((tanu ,(* 2 n)) ,x)]
    [`(tan (* ,(or 'PI '(PI)) ,x))
     #:when bftanu
     `((tanu 2) ,x)]
    [`(tan (* ,x ,(or 'PI '(PI))))
     #:when bftanu
     `((tanu 2) ,x)]
    [`(tan (* (* 2 ,(or 'PI '(PI))) ,x))
     #:when bftanu
     `((tanu 1) ,x)]
    [`(tan (* ,x (* 2 ,(or 'PI '(PI)))))
     #:when bftanu
     `((tanu 1) ,x)]

    ; Handle pow(x, 1/5) and similar
    [`(pow (fabs ,x) ,y) `(pow (fabs ,x) ,y)]
    [`(pow ,x ,(? rational? y))
     (cond
       [(integer? y) `(pow ,x ,y)] ; Not optimal but probably fine
       [(and (even? (numerator y)) (odd? (denominator y))) `(pow (fabs ,x) ,y)]
       [(and (odd? (numerator y)) (odd? (denominator y))) `(copysign (pow (fabs ,x) ,y) ,x)]
       [else `(pow ,x ,y)])]
    [_ expr]))

(define (exprs->batch exprs vars)
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars] [i (in-naturals)])
                 (cons var i))))
  ; Counts
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (define node ; This compiles to the register machine
      (match (optimize prog)
        [(list op args ...) (cons op (map munge args))]
        [_ prog]))
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define roots (list->vector (map munge exprs)))
  (define nodes (list->vector (reverse icache)))

  (values nodes roots))

(define +inf.bf (bf +inf.0))

(define (ival-infinity)
  (ival (bfprev +inf.bf) +inf.bf))

(define (ival-nan)
  ival-illegal)

(define (ival-true)
  (ival-bool #t))

(define (ival-false)
  (ival-bool #f))

(define (real->ival val)
  (define lo
    (parameterize ([bf-rounding-mode 'down])
      (bf val)))
  (define hi
    (parameterize ([bf-rounding-mode 'up])
      (bf val)))
  (ival lo hi))

(define (ival-point? x)
  (bf=? (ival-lo x) (ival-hi x)))

(define (ival-const x)
  (procedure-rename (const (real->ival x))
                    (if (*rival-name-constants*) (string->symbol (number->string x)) 'exact)))

(define (ival-rational x)
  (procedure-rename (lambda () (real->ival x))
                    (if (*rival-name-constants*) (string->symbol (number->string x)) 'const)))

(define (rival-compile exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots) (exprs->batch exprs vars))

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
                         ([node (in-vector nodes num-vars)])
      (fn->ival-fn node)))

  (define register-count (+ (length vars) (vector-length instructions)))
  (define registers (make-vector register-count))
  (define repeats (make-vector register-count #f)) ; flags whether an op should be evaluated
  (define precisions (make-vector register-count)) ; vector that stores working precisions
  ;; starting precisions for the first, un-tuned iteration
  (define initial-precisions (setup-vstart-precs instructions (length vars) roots discs))

  (rival-machine (list->vector vars)
                 instructions
                 roots
                 (list->vector discs)
                 registers
                 repeats
                 precisions
                 initial-precisions
                 (make-vector (vector-length roots))
                 0
                 0
                 0
                 (make-vector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))
                 (make-flvector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))))

; Function sets up vstart-precs vector, where all the precisions
; are equal to (+ (*base-tuning-precision*) (* depth (*ampl-tuning-bits*))),
; where depth is the depth of a node in the given computational tree (ivec)
(define (setup-vstart-precs ivec varc roots discs)
  (define ivec-len (vector-length ivec))
  (define vstart-precs (make-vector ivec-len 0))

  (for ([root (in-vector roots)] [disc (in-list discs)] #:when (>= root varc))
    (vector-set! vstart-precs
                 (- root varc)
                 (+ (discretization-target disc) (*base-tuning-precision*))))

  (for ([instr (in-vector ivec (- ivec-len 1) -1 -1)] ; reversed over ivec
        [n (in-range (- ivec-len 1) -1 -1)]) ; reversed over indices of vstart-precs
    (define current-prec (vector-ref vstart-precs n))

    (define tail-registers (cdr instr))
    (for ([idx (in-list tail-registers)] #:when (>= idx varc))
      (define idx-prec (vector-ref vstart-precs (- idx varc)))
      (vector-set! vstart-precs
                   (- idx varc)
                   (max ; sometimes an instruction can be in many tail registers
                    idx-prec ; We wanna make sure that we do not tune a precision down
                    (+ current-prec (*ampl-tuning-bits*))))))

  vstart-precs)

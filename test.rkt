#lang racket

(require racket/math math/base math/flonum math/bigfloat)
(require rackunit)
(require "main.rkt")

(define (ival-valid? ival)
  (if (ival-err ival)
      (ival-err? ival)
      (if (boolean? (ival-lo ival))
          (or (not (ival-lo ival)) (ival-hi ival))
          (bf<= (ival-lo ival) (ival-hi ival)))))

(define (ival-contains? ival pt)
  (if (bigfloat? pt)
      (if (bfnan? pt)
          (ival-err? ival)
          (and (not (ival-err ival))
               (bf<= (ival-lo ival) pt) (bf<= pt (ival-hi ival))))
      (and (not (ival-err ival))
           (or (equal? pt (ival-lo ival))
               (equal? pt (ival-hi ival))))))

(define (value-equals? bf1 bf2)
  (if (boolean? bf1)
      (equal? bf1 bf2)
      (or (bf= bf1 bf2) (and (bfnan? bf1) (bfnan? bf2)))))

(define (value-lte? bf1 bf2)
  (if (boolean? bf1)
      (or (not bf1) bf2)
      (bf<= bf1 bf2)))

(define (ival-refines? coarse fine)
  (and
   (or (ival-err fine)
       (and
        ((if (ival-lo-fixed? coarse) value-equals? value-lte?)
         (ival-lo coarse) (ival-lo fine))
        ((if (ival-hi-fixed? coarse) value-equals? value-lte?)
         (ival-hi fine) (ival-hi coarse))))
   (if (ival-lo-fixed? coarse) (ival-lo-fixed? fine) #t)
   (if (ival-hi-fixed? coarse) (ival-hi-fixed? fine) #t)
   (if (ival-err? fine) (ival-err? coarse) #t)
   (if (ival-err coarse) (ival-err fine) #t)))

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define (bfcopysign x y)
  (bf* (bfabs x) (if (= (bigfloat-signbit y) 1) -1.bf 1.bf)))

(define (bffdim x y)
  (if (bf> x y) (bf- x y) 0.bf))

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define (if-fn c x y)
  (if c x y))

(define (bffmod x mod)
  (cond
   [(bfinfinite? x) +nan.bf]
   [(bfinfinite? mod) x]
   [else
    ;; For this to be precise, we need enough bits
    (define precision
      (+ (bf-precision) (max (- (bigfloat-exponent x) (bigfloat-exponent mod)) 0)))
    (parameterize ([bf-precision precision]) 
      (bfcanonicalize (bf- x (bf* (bftruncate (bf/ x mod)) mod))))]))

(define (bffma a b c)
  ;; `bfstep` truncates to `(bf-precision)` bits
  (bfstep (bf+ c (parameterize ([bf-precision (* (bf-precision) 2)]) (bf* a b))) 0))

(define (bfremainder x mod)
  (define y (bffmod x mod))
  (define mod* (bfabs mod))
  (define mod/2 (bf/ mod* 2.bf))
  (cond
   [(bf> y mod/2) (bf- y mod*)]
   [(bf< y (bf- mod/2)) (bf+ y mod*)]
   [else y]))

(define function-table
  (list (list ival-neg   bf-        '(real) 'real)
        (list ival-fabs  bfabs      '(real) 'real)
        (list ival-sqrt  bfsqrt     '(real) 'real)
        (list ival-cbrt  bfcbrt     '(real) 'real)
        (list ival-exp   bfexp      '(real) 'real)
        (list ival-exp2  bfexp2     '(real) 'real)
        (list ival-expm1 bfexpm1    '(real) 'real)
        (list ival-log   bflog      '(real) 'real)
        (list ival-log2  bflog2     '(real) 'real)
        (list ival-log10 bflog10    '(real) 'real)
        (list ival-log1p bflog1p    '(real) 'real)
        (list ival-logb  bflogb     '(real) 'real)
        (list ival-sin   bfsin      '(real) 'real)
        (list ival-cos   bfcos      '(real) 'real)
        (list ival-tan   bftan      '(real) 'real)
        (list ival-asin  bfasin     '(real) 'real)
        (list ival-acos  bfacos     '(real) 'real)
        (list ival-atan  bfatan     '(real) 'real)
        (list ival-sinh  bfsinh     '(real) 'real)
        (list ival-cosh  bfcosh     '(real) 'real)
        (list ival-tanh  bftanh     '(real) 'real)
        (list ival-asinh bfasinh    '(real) 'real)
        (list ival-acosh bfacosh    '(real) 'real)
        (list ival-atanh bfatanh    '(real) 'real)
        (list ival-erf   bferf      '(real) 'real)
        (list ival-erfc  bferfc     '(real) 'real)
        (list ival-rint  bfrint     '(real) 'real)
        (list ival-round bfround    '(real) 'real)
        (list ival-ceil  bfceiling  '(real) 'real)
        (list ival-floor bffloor    '(real) 'real)
        (list ival-trunc bftruncate '(real) 'real)
        (list ival-tgamma bfgamma   '(real) 'real)
        (list ival-lgamma bflog-gamma '(real) 'real)
        (list ival-add   bf+      '(real real) 'real)
        (list ival-sub   bf-      '(real real) 'real)
        (list ival-mult  bf*      '(real real) 'real)
        (list ival-div   bf/      '(real real) 'real)
        (list ival-fma   bffma      '(real real real) 'real)
        (list ival-pow   bfexpt     '(real real) 'real)
        (list ival-hypot bfhypot    '(real real) 'real)
        (list ival-atan2 bfatan2    '(real real) 'real)
        (list ival-fmod  bffmod     '(real real) 'real)
        (list ival-remainder bfremainder '(real real) 'real)
        (list ival-<     bf<      '(real real) 'bool)
        (list ival-<=    bf<=     '(real real) 'bool)
        (list ival->     bf>      '(real real) 'bool)
        (list ival->=    bf>=     '(real real) 'bool)
        (list ival-==    bf=       '(real real) 'bool)
        (list ival-!= (compose not bf=) '(real real) 'bool)
        (list ival-fmin  bfmin     '(real real) 'real)
        (list ival-fmax  bfmax     '(real real) 'real)
        (list ival-copysign bfcopysign '(real real) 'real)
        (list ival-fdim  bffdim     '(real real) 'real)
        (list ival-and   and-fn     '(bool bool bool) 'bool)
        (list ival-or    or-fn      '(bool bool bool) 'bool)
        (list ival-not   not        '(bool) 'bool)
        (list ival-if    if-fn      '(bool real real) 'real)
        ))

(define (sample-bigfloat)
  (define exponent (random -1023 1023)) ; Pretend-double
  (define significand (bf (random-bits (bf-precision)) (- (bf-precision))))
  (define val (bfshift (bf+ 1.bf significand) exponent))
  (if (= (random 0 2) 1) (bf- val) val))

(define (sample-wide-interval)
  (define v1 (sample-bigfloat))
  (define v2 (sample-bigfloat))
  (ival (bfmin v1 v2) (bfmax v1 v2)))

(define (sample-narrow-interval)
  ;; Biased toward small intervals
  (define v1 (sample-bigfloat))
  (define size (random 1 (bf-precision)))
  (define delta (* (match (random 0 2) [0 -1] [1 1]) size))
  (define v2 (bfstep v1 delta))
  (ival (bfmin v1 v2) (bfmax v1 v2)))

(define (sample-interval type)
  (match type
    ['real
     (define x (if (= (random 0 2) 0) (sample-wide-interval) (sample-narrow-interval)))
     (if (or (bfnan? (ival-lo x)) (bfnan? (ival-hi x))) (sample-interval type) x)]
    ['bool
     (match (random 0 3)
       [0 (ival #f)]
       [1 (ival #t)]
       [2 (ival #f #t)])]))

(define (sample-from ival)
  (if (bigfloat? (ival-lo ival))
      (if (bf<= (ival-lo ival) 0.bf (ival-hi ival))
          (let* ([p (random)]
                 [lo* (bigfloat->flonum (ival-lo ival))]
                 [hi* (bigfloat->flonum (ival-hi ival))]
                 [range (flonums-between lo* hi*)])
              (bf (flstep lo* (exact-floor (* p range)))))
          (let ([p (random)] [range (bigfloats-between (ival-lo ival) (ival-hi ival))])
            (bfstep (ival-lo ival) (exact-floor (* p range)))))
      (let ([p (random 0 2)])
        (if (= p 0) (ival-lo ival) (ival-hi ival)))))

(define-simple-check (check-ival-valid? ival)
  (ival-valid? ival))

(define-binary-check (check-ival-equals? ival1 ival2)
  (if (ival-err ival1)
      (ival-err ival2)
      (and (value-equals? (ival-lo ival1) (ival-lo ival2))
           (value-equals? (ival-hi ival1) (ival-hi ival2)))))

(define num-tests 250)
(define num-slow-tests 100)
(define num-witnesses 10)
(define slow-tests (list ival-lgamma ival-tgamma))

(define (test-entry ival-fn fn args)
  (define is (for/list ([arg args]) (sample-interval arg)))
  (define iy (apply ival-fn is))

  (with-check-info (['intervals is])
    (check-ival-valid? iy))

  (define xs #f)
  (define y #f)
  (for ([_ (in-range num-witnesses)])
    (set! xs (for/list ([i is]) (sample-from i)))
    (set! y (apply fn xs))
    (with-check-info (['intervals is] ['points xs])
      (check ival-contains? iy y)))

  (with-check-info (['intervals is] ['points xs] ['iy iy] ['y y])
    (for ([k (in-naturals)] [i is] [x xs])
      (define-values (ilo ihi) (ival-split i x))
      (when (and ilo ihi)
        (define iylo (apply ival-fn (list-set is k ilo)))
        (define iyhi (apply ival-fn (list-set is k ihi)))
        (with-check-info (['split-argument k] ['ilo ilo] ['ihi ihi] ['iylo iylo] ['iyhi iyhi])
          (check-ival-equals? iy (ival-union iylo iyhi)))))
    (when (or (ival-lo-fixed? iy) (ival-hi-fixed? iy))
      (define iy* (parameterize ([bf-precision 128]) (apply ival-fn is)))
      (check ival-refines? iy iy*))))

(define (run-tests)
  (check ival-contains? (ival-bool #f) #f)
  (check ival-contains? (ival-bool #t) #t)
  (check ival-contains? (ival-pi) pi.bf)
  (check ival-contains? (ival-e) (bfexp 1.bf))
  (test-case "mk-ival"
    (for ([i (in-range num-tests)])
      (define pt (sample-bigfloat))
      (with-check-info (['point pt])
        (check-ival-valid? (mk-ival pt))
        (check ival-contains? (mk-ival pt) pt))))

  (test-case "ival-error?"
    (let* ([ival1 (mk-ival 1.bf)] [ival0 (mk-ival 0.bf)] [ivalboth (ival 0.bf 1.bf)]
           [res1 (ival-div ival1 ival1)] [res2 (ival-div ival1 ival0)] [res3 (ival-div ival1 ivalboth)])
      (check-ival-valid? (ival-error? res1))
      (check-ival-valid? (ival-error? res2))
      (check-ival-valid? (ival-error? res3))
      (check ival-contains? (ival-error? res1) #f)
      (check ival-contains? (ival-error? res2) #t)
      (check ival-contains? (ival-error? res3) #f)
      (check ival-contains? (ival-error? res3) #t)))

  (define (sorted? list cmp)
    (cond
      [(<= (length list) 1) #t]
      [else
       (and (cmp (first list) (second list))
            (sorted? (rest list) cmp))]))
  
  (test-case "ival-sort"
    (for ([n (in-range num-tests)])
         (let* ([input (for/list ([n (in-range (random 0 30))])
                                 (sample-interval 'real))]
                [output (ival-sort input bf<)])
           (check-true (sorted? (map ival-lo output) bf<=))
           (check-true (sorted? (map ival-hi output) bf<=)))))

  (for ([entry (in-list function-table)])
    (match-define (list ival-fn fn args _) entry)
    (define N (if (memq ival-fn slow-tests) num-slow-tests num-tests))
    (eprintf "~a on ~a inputs: " (object-name ival-fn) N)
    (define start-time (current-inexact-milliseconds))
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range N)])
         (test-entry ival-fn fn args)
         (when (= (remainder n 10) 0)
           (eprintf "."))))
    (define dt (/ (- (current-inexact-milliseconds) start-time) N))
    (eprintf " ~ams each" (~r dt #:min-width 6 #:precision '(= 3)))
    (eprintf "\n")))

(module+ test (run-tests))
(module+ main
  (require racket/cmdline)
  (command-line
   #:args (fname [n (~a num-tests)])
   (define entry
     (findf (λ (entry) (equal? (~a (object-name (first entry))) (format "ival-~a" fname)))
            function-table))
   (match entry
     [#f
      (raise-user-error 'test.rkt "No function named ival-~a" fname)]
     [(list ival-fn fn itypes otype)
      (for ([n (in-range (string->number n))])
        (test-entry ival-fn fn itypes)
        (eprintf "."))
      (newline)])))

#lang racket/base

(require racket/contract racket/match racket/function math/private/bigfloat/mpfr racket/list)
(require (for-syntax racket/base))
(module+ test (require rackunit))

(define *rival-precision* (make-parameter (expt 2 20)))

(define-match-expander ival-expander
  (λ (stx)
    (syntax-case stx ()
      [(_me lo hi)
       #'(ival (endpoint lo _) (endpoint hi _) _ _)]
      [(_me name lo hi)
       #'(and name (ival (endpoint lo _) (endpoint hi _) _ _))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ lo)
       #'(mk-big-ival lo lo)]
      [(_ lo hi)
       #'(mk-big-ival lo hi)])))

(struct endpoint (val immovable?) #:transparent)
(struct ival (lo hi err? err) #:transparent
 #:methods gen:custom-write
 [(define (write-proc ival port mode)
    (if (ival-err ival)
        (fprintf port "ival-illegal")
        (fprintf port "(ival ~s ~s)" (ival-lo-val ival) (ival-hi-val ival))))])

(define ival-list? (listof ival?))
(define value? (or/c bigfloat? boolean?))

(define (ival-hi-val ival)
  (endpoint-val (ival-hi ival)))
(define (ival-lo-val ival)
  (endpoint-val (ival-lo ival)))
(define (ival-lo-fixed? ival)
  (endpoint-immovable? (ival-lo ival)))
(define (ival-hi-fixed? ival)
  (endpoint-immovable? (ival-hi ival)))

(provide ival? (rename-out [ival-expander ival] [ival-hi-val ival-hi] [ival-lo-val ival-lo])
         (rename-out [monotonic monotonic->ival] [comonotonic comonotonic->ival])
         (contract-out
          [ival-union (-> ival? ival? ival?)]
          [ival-split (-> ival? value? (values (or/c ival? #f) (or/c ival? #f)))])
         (contract-out
          [ival-pi (-> ival?)]
          [ival-e  (-> ival?)]
          [ival-bool (-> boolean? ival?)]
          [ival-add (-> ival? ival? ival?)]
          [ival-sub (-> ival? ival? ival?)]
          [ival-neg (-> ival? ival?)]
          [ival-mult (-> ival? ival? ival?)]
          [ival-div (-> ival? ival? ival?)]
          [ival-fma (-> ival? ival? ival? ival?)] ; TODO: untested
          [ival-fabs (-> ival? ival?)]
          [ival-sqrt (-> ival? ival?)]
          [ival-cbrt (-> ival? ival?)]
          [ival-hypot (-> ival? ival? ival?)]
          [ival-exp (-> ival? ival?)]
          [ival-exp2 (-> ival? ival?)]
          [ival-expm1 (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
          [ival-log2 (-> ival? ival?)]
          [ival-log10 (-> ival? ival?)]
          [ival-log1p (-> ival? ival?)]
          [ival-logb (-> ival? ival?)]
          [ival-pow (-> ival? ival? ival?)]
          [ival-sin (-> ival? ival?)]
          [ival-cos (-> ival? ival?)]
          [ival-tan (-> ival? ival?)]
          [ival-asin (-> ival? ival?)]
          [ival-acos (-> ival? ival?)]
          [ival-atan (-> ival? ival?)]
          [ival-atan2 (-> ival? ival? ival?)]
          [ival-sinh (-> ival? ival?)]
          [ival-cosh (-> ival? ival?)]
          [ival-tanh (-> ival? ival?)]
          [ival-asinh (-> ival? ival?)]
          [ival-acosh (-> ival? ival?)]
          [ival-atanh (-> ival? ival?)]
          [ival-erf (-> ival? ival?)]
          [ival-erfc (-> ival? ival?)]
          [ival-lgamma (-> ival? ival?)]
          [ival-tgamma (-> ival? ival?)]
          [ival-fmod (-> ival? ival? ival?)]
          [ival-remainder (-> ival? ival? ival?)]
          [ival-rint (-> ival? ival?)]
          [ival-round (-> ival? ival?)]
          [ival-ceil (-> ival? ival?)]
          [ival-floor (-> ival? ival?)]
          [ival-trunc (-> ival? ival?)]
          [ival-fmin (-> ival? ival? ival?)]
          [ival-fmax (-> ival? ival? ival?)]
          [ival-copysign (-> ival? ival? ival?)]
          [ival-fdim (-> ival? ival? ival?)]
          [ival-sort (-> ival-list? (-> value? value? boolean?) ival-list?)])
         (contract-out
          [ival-<  (->* () #:rest (listof ival?) ival?)]
          [ival-<= (->* () #:rest (listof ival?) ival?)]
          [ival->  (->* () #:rest (listof ival?) ival?)]
          [ival->= (->* () #:rest (listof ival?) ival?)]
          [ival-== (->* () #:rest (listof ival?) ival?)]
          [ival-!= (->* () #:rest (listof ival?) ival?)]
          [ival-if (-> ival? ival? ival? ival?)]
          [ival-and (->* () #:rest (listof ival?) ival?)]
          [ival-or  (->* () #:rest (listof ival?) ival?)]
          [ival-not (-> ival? ival?)])
         (contract-out
          [ival-error? (-> ival? ival?)]
          [ival-illegal ival?]
          [ival-assert (->* (ival?) (identity) ival?)]
          [ival-then (->* (ival?) #:rest (listof ival?) ival?)])
         close-enough->ival
         ; Deprecated
         ival-lo-fixed? ival-hi-fixed? ival-err? ival-err mk-ival
         )

(define -inf.bf (bf -inf.0))
(define -1.bf (bf -1))
(define 0.bf (bf 0))
(define half.bf (bf 0.5))
(define 1.bf (bf 1))
(define 2.bf (bf 2))
(define 3.bf (bf 3))
(define +inf.bf (bf +inf.0))
(define +nan.bf (bf +nan.0))

(define (mk-big-ival x y)
  (cond
   [(and (bigfloat? x) (bigfloat? y))
    (define fix? (bf=? x y))
    (define err? (or (bfnan? x) (bfnan? y)
                     (and (bfinfinite? x) fix?)))
    (ival (endpoint x fix?) (endpoint y fix?) err? err?)]
   [(and (boolean? x) (boolean? y))
    (define fix? (equal? x y))
    (ival (endpoint x fix?) (endpoint y fix?) #f #f)]
   [else
    (error 'ival "Invalid interval endpoints" x y)]))

(define (mk-ival x)
  (mk-big-ival x x))

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define (ival-pi)
  (ival (endpoint (rnd 'down pi.bf) #f) (endpoint (rnd 'up pi.bf) #f) #f #f))

(define (ival-e)
  (ival (endpoint (rnd 'down bfexp 1.bf) #f) (endpoint (rnd 'up bfexp 1.bf) #f) #f #f))

(define (ival-bool b)
  (ival (endpoint b #t) (endpoint b #t) #f #f))

(define ival-true (ival-bool #t))
(define ival-false (ival-bool #f))
(define ival-uncertain (ival (endpoint #f #f) (endpoint #t #f) #f #f))
(define ival-illegal (ival (endpoint +nan.bf #t) (endpoint +nan.bf #t) #t #t))

(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define (split-ival i val)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) i)
  (values (struct-copy ival i [hi (endpoint val xhi!)])
          (struct-copy ival i [lo (endpoint val xlo!)])))

(define (ival-split i val)
  (cond
   [(boolean? val) (if val (values i #f) (values #f i))]
   [(bflte? (ival-hi-val i) val) (values i #f)]
   [(bfgte? (ival-lo-val i) val) (values #f i)]
   [else (split-ival i val)]))

(define (classify-ival x [val #f])
  (when val (set! x (ival-sub x (ival-expander val))))
  (if (ival-err x)
      1
      (- 1 (bigfloat-signbit (ival-lo-val x)) (bigfloat-signbit (ival-hi-val x)))))

(define (classify-ival-strict x [val 0.bf])
  (cond [(bfgt? (ival-lo-val x) val) 1] [(bflt? (ival-hi-val x) val) -1] [else 0]))

(define (endpoint-min2 e1 e2)
  (match-define (endpoint x x!) e1)
  (match-define (endpoint y y!) e2)
  (define out (bfmin2 x y))
  (endpoint out (or (and (bf=? out x) x!) (and (bf=? out y) y!))))

(define (endpoint-max2 e1 e2)
  (match-define (endpoint x x!) e1)
  (match-define (endpoint y y!) e2)
  (define out (bfmax2 x y))
  (endpoint out (or (and (bf=? out x) x!) (and (bf=? out y) y!))))

(define (ival-union x y)
  (cond
   [(ival-err x) (struct-copy ival y [err? #t])]
   [(ival-err y) (struct-copy ival x [err? #t])]
   [(bigfloat? (ival-lo-val x))
    (ival (rnd 'down endpoint-min2 (ival-lo x) (ival-lo y))
          (rnd 'up endpoint-max2 (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (and (ival-err x) (ival-err y)))]
   [(boolean? (ival-lo-val x))
    (ival (epfn and-fn (ival-lo x) (ival-lo y))
          (epfn or-fn (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (and (ival-err x) (ival-err y)))]))

;; This function computes and propagates the immovable? flag for endpoints
(define (epfn op . args)
  (define args-bf (map endpoint-val args))
  (define-values (result exact?) (bf-return-exact? op args-bf))
  (endpoint result (and (andmap endpoint-immovable? args) exact?)))

;; Some hairy code follows to access the MPFR "inexact" exception.
;; It assumes no one else cares about the flag, so it clobbers it.
(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide mpfr_clear_inexflag mpfr_get_inexflag)
  (define mpfr_clear_inexflag (get-mpfr-fun 'mpfr_clear_inexflag (_fun -> _void)))
  (define mpfr_get_inexflag (get-mpfr-fun 'mpfr_inexflag_p (_fun -> _int))))
(require (submod "." hairy))

(define (bf-return-exact? op args)
  (mpfr_clear_inexflag)
  (define out (apply op args))
  (define exact? (= (mpfr_get_inexflag) 0))
  (values out exact?))
;; End hairy code

;; Endpoint computation for both `add`, `sub`, and `hypot` (which has an add inside)
(define (eplinear bffn a-endpoint b-endpoint)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bffn (list a b)))
  (endpoint val (or (and a! b! exact?) (and a! (bfinfinite? a)) (and b! (bfinfinite? b)))))

(define (ival-add x y)
  (ival
   (rnd 'down eplinear bfadd (ival-lo x) (ival-lo y))
   (rnd 'up   eplinear bfadd (ival-hi x) (ival-hi y))
   (or (ival-err? x) (ival-err? y))
   (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down eplinear bfsub (ival-lo x) (ival-hi y))
        (rnd 'up   eplinear bfsub (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (epmul a-endpoint b-endpoint a-class b-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?)
    (if (or (bfzero? a) (bfzero? b))
        (values 0.bf #t) ; 0 * inf = 0, not nan, because inf is potential, not actual
        (bf-return-exact? bfmul (list a b))))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bfzero? a))
       (and a! (bfinfinite? a) (not (= b-class 0)))
       (and b! (bfzero? b))
       (and b! (bfinfinite? b) (not (= a-class 0))))))

(define (ival-mult x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)

  (define (mkmult a b c d)
    (ival (rnd 'down epmul a b x-sign y-sign)
          (rnd 'up   epmul c d x-sign y-sign)
          (or xerr? yerr?)  (or xerr yerr)))

  (define x-sign (classify-ival x))
  (define y-sign (classify-ival y))

  (match* (x-sign y-sign)
    [( 1  1) (mkmult xlo ylo xhi yhi)]
    [( 1 -1) (mkmult xhi ylo xlo yhi)]
    [( 1  0) (mkmult xhi ylo xhi yhi)]
    [(-1  0) (mkmult xlo yhi xlo ylo)]
    [(-1  1) (mkmult xlo yhi xhi ylo)]
    [(-1 -1) (mkmult xhi yhi xlo ylo)]
    [( 0  1) (mkmult xlo yhi xhi yhi)]
    [( 0 -1) (mkmult xhi ylo xlo ylo)]
    [( 0  0)
     ;; Here, the two branches of the union are meaningless on their own;
     ;; however, both branches compute possible lo/hi's to min/max together
     (ival-union (mkmult xhi ylo xlo ylo)
                 (mkmult xlo yhi xhi yhi))]))

(define (epdiv a-endpoint b-endpoint a-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bfdiv (list a b)))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bfzero? a))
       (and a! (bfinfinite? a))
       (and b! (bfinfinite? b))
       (and b! (bfzero? b) (not (= a-class 0))))))

(define (ival-div x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr? (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or xerr yerr (and (bfzero? (ival-lo-val y)) (bfzero? (ival-hi-val y)))))
  (define x-class (classify-ival-strict x))
  (define y-class (classify-ival-strict y))

  (define (mkdiv a b c d)
    (ival (rnd 'down epdiv a b x-class) (rnd 'up epdiv c d x-class) err? err))
  
  (match* (x-class y-class)
    [(_ 0) ; In this case, y stradles 0
     (define immovable? (and (endpoint-immovable? xlo) (endpoint-immovable? ylo)))
     (ival (endpoint -inf.bf immovable?) (endpoint +inf.bf immovable?) err? err)]
    [( 1  1) (mkdiv xlo yhi xhi ylo)]
    [( 1 -1) (mkdiv xhi yhi xlo ylo)]
    [(-1  1) (mkdiv xlo ylo xhi yhi)]
    [(-1 -1) (mkdiv xhi ylo xlo yhi)]
    [( 0  1) (mkdiv xlo ylo xhi ylo)]
    [( 0 -1) (mkdiv xhi yhi xlo yhi)]))

;; Helpers for defining interval functions

(define-syntax-rule (define* name expr)
  (define name (procedure-rename expr 'name)))

(define ((monotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival (rnd 'down epfn bffn lo) (rnd 'up epfn bffn hi) err? err))

(define ((comonotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival (rnd 'down epfn bffn hi) (rnd 'up epfn bffn lo) err? err))

(define ((close-enough->ival bffn) x)
  (match-define (ival (endpoint lo lo!) (endpoint hi hi!) err? err) x)
  (define close-enough? (bffn lo hi))
  (ival (endpoint close-enough? #f)
        (endpoint (or (not lo!) (not hi!) close-enough?) #f)
        err?
        err))

(define ((clamp lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (define err? (or xerr? (bflt? xlo lo) (bfgt? xhi hi)))
  (define err (or (or xerr (bflt? xhi lo) (bfgt? xlo hi))))
  
  (if (and (bfzero? lo) (bfzero? xhi))
      (ival (endpoint 0.bf xlo!) (endpoint 0.bf xhi!) err? err)
      (ival (endpoint (if (bflt? xlo lo) lo xlo) xlo!)
            (endpoint (if (bfgt? xhi hi) hi xhi) xhi!)
            err?
            err)))

(define ((clamp-strict lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (ival (endpoint (if (bflt? xlo lo) lo xlo) xlo!)
        (endpoint (if (bfgt? xhi hi) hi xhi) xhi!)
        (or xerr? (bflte? xlo lo) (bfgte? xhi hi))
        (or xerr (bflte? xhi lo) (bfgte? xlo hi))))

(define ((overflows-at fn lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match-define (ival (endpoint ylo ylo!) (endpoint yhi yhi!) yerr? yerr) (fn x))
  (ival (endpoint ylo (or ylo! (bflte? xhi lo) (and (bflte? xlo lo) xlo!)))
        (endpoint yhi (or yhi! (bfgte? xlo hi) (and (bfgte? xhi hi) xhi!)))
        xerr? xerr))

(define* ival-neg (comonotonic bfneg))

;; This function fixes a bug in MPFR where mixed-precision
;; rint/round/ceil/floor/trunc operations are rounded in the input
;; precision, not the output precision, so (rnd 'down bfround xxx) can
;; return +inf.bf
(define ((fix-rounding f) x)
  (if (>= (bigfloat-exponent x) 0)
      (bfrint x)
      (f x)))

(define* ival-rint (monotonic bfrint))
(define* ival-round (monotonic (fix-rounding bfround)))
(define* ival-ceil (monotonic (fix-rounding bfceiling)))
(define* ival-floor (monotonic (fix-rounding bffloor)))
(define* ival-trunc (monotonic (fix-rounding bftruncate)))

(define (ival-fabs x)
  (match (classify-ival x)
    [-1 ((comonotonic bfabs) x)]
    [1 ((monotonic bfabs) x)]
    [0
     (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
     (ival (endpoint 0.bf (and xlo! xhi!))
           (rnd 'up endpoint-max2 (epfn bfabs (ival-lo x)) (ival-hi x))
           (ival-err? x) (ival-err x))]))

;; Since MPFR has a cap on exponents, no value can be more than twice MAX_VAL
(define exp-overflow-threshold  (bfadd (bflog (bfprev +inf.bf)) 1.bf))
(define exp2-overflow-threshold (bfadd (bflog2 (bfprev +inf.bf)) 1.bf))

(define* ival-exp
  (overflows-at (monotonic bfexp) (bfneg exp-overflow-threshold) exp-overflow-threshold))
(define* ival-expm1
  (overflows-at (monotonic bfexpm1) (bfneg exp-overflow-threshold) exp-overflow-threshold))
(define* ival-exp2
  (overflows-at (monotonic bfexp2) (bfneg exp2-overflow-threshold) exp2-overflow-threshold))

(define* ival-log (compose (monotonic bflog) (clamp-strict 0.bf +inf.bf)))
(define* ival-log2 (compose (monotonic bflog2) (clamp-strict 0.bf +inf.bf)))
(define* ival-log10 (compose (monotonic bflog10) (clamp-strict 0.bf +inf.bf)))
(define* ival-log1p (compose (monotonic bflog1p) (clamp-strict -1.bf +inf.bf)))
(define* ival-logb (compose ival-floor ival-log2 ival-fabs))

(define* ival-sqrt (compose (monotonic bfsqrt) (clamp 0.bf +inf.bf)))
(define* ival-cbrt (monotonic bfcbrt))

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-fabs x))
  (define y* (ival-fabs y))
  (ival (rnd 'down eplinear bfhypot (ival-lo x*) (ival-lo y*))
        (rnd 'up   eplinear bfhypot (ival-hi x*) (ival-hi y*)) err? err))

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
  (define x-class (classify-ival x 1.bf))
  (define y-class (classify-ival y))

  (define (mk-pow a b c d)
    (match-define (endpoint lo lo!) (rnd 'down eppow a b x-class y-class))
    (match-define (endpoint hi hi!) (rnd 'up   eppow c d x-class y-class))
    (define out
      (ival (endpoint lo lo!) (endpoint hi hi!)
            (or xerr? yerr? (and (bfzero? (endpoint-val xlo)) (not (= y-class 1))))
            (or xerr yerr (and (bfzero? (endpoint-val xhi)) (= y-class -1)))))
    (if (or (bfzero? lo) (bfinfinite? lo) (bfzero? hi) (bfinfinite? hi))
      (ival-copy-movability out (ival-exp (ival-mult y (ival-log x))))
      out))

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
              (ival-neg (ival-pow-pos (ival-fabs x) y)) ; Use fabs in case of [x, 0]
              (ival-pow-pos (ival-fabs x) y))
          ; If y is non-integer point interval, it must be an even
          ; fraction (because all bigfloats are) so we always error
          ival-illegal)
      ; Moreover, if we have (-x)^y, that's basically x^y U -(x^y).
      (let ([pospow (ival-pow-pos (ival-fabs x) y)])
        (ival-then (ival-assert ival-maybe) (ival-union (ival-neg pospow) pospow)))))

(define* ival-pow2 (compose (monotonic (lambda (x) (bfmul x x))) ival-fabs))

(define (ival-pow x y)
  (cond
   [(and (bf=? (ival-hi-val y) 2.bf) (bf=? (ival-lo-val y) 2.bf))
    (ival-pow2 x)]
   [(bflt? (ival-hi-val x) 0.bf) (ival-pow-neg x y)]
   [(bfgte? (ival-lo-val x) 0.bf) (ival-pow-pos x y)]
   [else
    (define-values (neg pos) (split-ival x 0.bf))
    (ival-union (ival-pow-neg neg y) (ival-pow-pos pos y))]))

(define (ival-fma a b c)
  (ival-add (ival-mult a b) c))

(define (ival-and . as)
  (ival (endpoint (andmap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (andmap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-or . as)
  (ival (endpoint (ormap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (ormap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-not x)
  (ival (epfn not (ival-hi x))
        (epfn not (ival-lo x))
        (ival-err? x)
        (ival-err x)))

(define (classify-ival-periodic x period)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (define lo-ulp (bigfloat-exponent xlo))
  (define hi-ulp (bigfloat-exponent xhi))
  (define lo-exp (+ lo-ulp (bigfloat-precision xlo)))
  (define hi-exp (+ hi-ulp (bigfloat-precision xhi)))
  (cond
    [(or (bfinfinite? xlo) (bfinfinite? xhi)) 'too-wide]
    [(and (< lo-exp 0) (< hi-exp 0)) 'near-0]
    [(or  (> lo-ulp 0) (> hi-ulp 0)) (if (bf=? xlo xhi) 'range-reduce 'too-wide)]
    [else 'range-reduce]))

(define (ival-cos x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match (classify-ival-periodic x '2pi)
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0
     (if (equal? (bigfloat-signbit xlo)
                 (bigfloat-signbit xhi))
         (if (> (bigfloat-signbit xlo) 0)
             ((monotonic bfcos) x)    ; negative
             ((comonotonic bfcos) x)) ; positive
         (ival (rnd 'down epfn bfmin2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
               (endpoint 1.bf #f) (ival-err? x) (ival-err x)))]
    ['range-reduce
     (let ([prec (min (*rival-precision*)
                      (max (bf-precision)
                           (max
                            (+ (bigfloat-exponent xlo) (bigfloat-precision xlo) (bigfloat-precision xlo))
                            (+ (bigfloat-exponent xhi) (bigfloat-precision xhi) (bigfloat-precision xhi)))))])
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision prec])
           (ival-floor (ival-div x (ival-pi)))))
       (cond
         [(and (bf=? a b) (bfeven? a))
          ((comonotonic bfcos) x)]
         [(and (bf=? a b) (bfodd? a))
          ((monotonic bfcos) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (ival (endpoint -1.bf #f)
                (rnd 'up epfn bfmax2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
                (ival-err? x) (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (ival (rnd 'down epfn bfmin2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
                (endpoint 1.bf #f) (ival-err? x) (ival-err x))]
         [else
          (ival-then x (mk-big-ival -1.bf 1.bf))]))]))

(define (ival-sin x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  
  (match (classify-ival-periodic x '2pi)
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0 ((monotonic bfsin) x)]
    ['range-reduce
     (let ([prec (min (*rival-precision*)
                      (max (bf-precision)
                           (max
                            (+ (bigfloat-exponent xlo) (bigfloat-precision xlo) (bigfloat-precision xlo))
                            (+ (bigfloat-exponent xhi) (bigfloat-precision xhi) (bigfloat-precision xhi)))))])
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision prec])
           (ival-round (ival-div x (ival-pi)))))
       (cond
         [(and (bf=? a b) (bfodd? a))
          ((comonotonic bfsin) x)]
         [(and (bf=? a b) (bfeven? a))
          ((monotonic bfsin) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (ival (endpoint -1.bf #f)
                (rnd 'up epfn bfmax2 (epfn bfsin (ival-lo x)) (epfn bfsin (ival-hi x)))
                (ival-err? x)
                (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (ival (rnd 'down epfn bfmin2 (epfn bfsin (ival-lo x)) (epfn bfsin (ival-hi x)))
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))]
         [else
          (ival-then x (mk-big-ival -1.bf 1.bf))]))]))

(define (ival-tan x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  
  (match (classify-ival-periodic x 'pi)
    ['too-wide (ival-then x (ival-assert (mk-big-ival #f #t) 'ival-tan) (mk-big-ival -inf.bf +inf.bf))]
    ['near-0 ((monotonic bftan) x)]
    ['range-reduce
     (let ([prec (min (*rival-precision*)
                      (max (bf-precision)
                           (max
                            (+ (bigfloat-exponent xlo) (bigfloat-precision xlo) (bigfloat-precision xlo))
                            (+ (bigfloat-exponent xhi) (bigfloat-precision xhi) (bigfloat-precision xhi)))))])
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision prec])
           (ival-round (ival-div x (ival-pi)))))
  
       (if (bf=? a b) ; Same period
           ((monotonic bftan) x)
           (ival-then x (ival-assert (mk-big-ival #f #t) 'ival-tan) (mk-big-ival -inf.bf +inf.bf))))]))

(define* ival-asin (compose (monotonic bfasin) (clamp -1.bf 1.bf)))
(define* ival-acos (compose (comonotonic bfacos) (clamp -1.bf 1.bf)))
(define* ival-atan (monotonic bfatan))

(define (ival-atan2 y x)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)

  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))

  (define (mkatan a b c d)
    (ival (rnd 'down epfn bfatan2 a b) (rnd 'up epfn bfatan2 c d) err? err))

  (match* ((classify-ival-strict x) (classify-ival-strict y))
    [(-1 -1) (mkatan yhi xlo ylo xhi)]
    [( 0 -1) (mkatan yhi xlo yhi xhi)]
    [( 1 -1) (mkatan ylo xlo yhi xhi)]
    [( 1  0) (mkatan ylo xlo yhi xlo)]
    [( 1  1) (mkatan ylo xhi yhi xlo)]
    [( 0  1) (mkatan ylo xhi ylo xlo)]
    [(-1  1) (mkatan yhi xhi ylo xlo)]
    [( _  0)
     (ival (endpoint (bfneg (rnd 'up pi.bf)) #f) (endpoint (rnd 'up pi.bf) #f)
            (or err? (bfgte? (ival-hi-val x) 0.bf))
            (or err (and (bf=? (ival-lo-val x) 0.bf) (bf=? (ival-hi-val x) 0.bf)
                         (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))]))

(define* ival-cosh (compose (monotonic bfcosh) ival-fabs))
(define* ival-sinh (monotonic bfsinh))
(define* ival-tanh (monotonic bftanh))
(define* ival-asinh (monotonic bfasinh))
(define* ival-acosh (compose (monotonic bfacosh) (clamp 1.bf +inf.bf)))
(define* ival-atanh (compose (monotonic bfatanh) (clamp-strict -1.bf 1.bf)))

(define (bfmul* a b)
  (if (or (bfzero? a) (bfzero? b)) 0.bf (bfmul a b)))

(define (ival-fmod-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define a (rnd 'down bftruncate (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
   [(bf=? a b) ; No intersection along `y.hi` edge
    (define c (rnd 'down bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
    (define d (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-lo-val y))))
    (cond
     [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
      (define lo (rnd 'down bfsub (ival-lo-val x) (rnd 'up bfmul* c (ival-hi-val y))))
      (define hi (rnd 'up bfsub (ival-hi-val x) (rnd 'down bfmul* c (ival-lo-val y))))
      (ival (endpoint lo #f)
            (endpoint hi #f)
            err? err)]
     [else
      (ival (endpoint 0.bf #f)
            (endpoint (rnd 'up bfmax2 (bfdiv (ival-hi-val x) (bfadd c 1.bf)) 0.bf) #f) err? err)])]
   [else
    (ival (endpoint 0.bf #f) (endpoint (ival-hi-val y) #f) err? err)]))

(define (ival-max-precision x)
  (max (bf-precision) (bigfloat-precision (ival-lo-val x)) (bigfloat-precision (ival-hi-val x))))

(define (ival-fmod x y)
  (define err? (or (ival-err? x) (ival-err? y)
                   (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y)
                  (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (parameterize ([bf-precision (max (ival-max-precision x)
                                    (ival-max-precision y))])
    (define y* (ival-fabs y))
    (cond
      [(bflte? (ival-hi-val x) 0.bf)
       (ival-neg (ival-fmod-pos
                  (ival-neg x)
                  y* err? err))]
      [(bfgte? (ival-lo-val x) 0.bf)
       (ival-fmod-pos x y* err? err)]
      [else
       (define-values (neg pos) (split-ival x 0.bf))
       (ival-union (ival-fmod-pos pos y* err? err)
                   (ival-neg (ival-fmod-pos
                              (ival-neg neg)
                              y* err? err)))])))

(define (ival-remainder-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define a (rnd 'down bfround (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
   [(bf=? a b) ; No intersection along `y.hi` edge
    (define c (rnd 'down bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
    (define d (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-lo-val y))))
    (cond
     [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
      (define y* (bfdiv (ival-hi-val y) 2.bf))
      (ival (endpoint (rnd 'down bfmax2 (bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y)))
                              (bfneg y*)) #f)
            (endpoint (rnd 'up bfmin2 (bfsub (ival-hi-val x) (rnd 'down bfmul c (ival-lo-val y)))
                              y*) #f)
            err? err)]
     [else
      ;; NOPE! need to subtract half.bf one way, add it another!
      (define y*-hi (rnd 'up bfdiv (bfdiv (ival-hi-val x) (bfadd c half.bf)) 2.bf))
      (define y*-lo (rnd 'down bfmax2
                         (bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y)))
                         (bfneg (bfdiv (ival-hi-val y) 2.bf))))
      (ival (endpoint (rnd 'down bfmin2 y*-lo (bfneg y*-hi)) #f) (endpoint y*-hi #f) err? err)])]
   [else
    (define y* (rnd 'up bfdiv (ival-hi-val y) 2.bf))
    (ival (endpoint (rnd 'down bfneg y*) #f) (endpoint y* #f) err? err)]))

;; Seems unnecessary
(define (ival-remainder x y)
  (define err? (or (ival-err? x) (ival-err? y)
                   (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y)
                  (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-fabs y))
  (cond
   [(bflte? (ival-hi-val x) 0.bf)
    (ival-neg (ival-remainder-pos (ival-neg x) y* err? err))]
   [(bfgte? (ival-lo-val x) 0.bf)
    (ival-remainder-pos x y* err? err)]
   [else
    (define-values (neg pos) (split-ival x 0.bf))
    (ival-union (ival-remainder-pos pos y* err? err)
                (ival-neg (ival-remainder-pos (ival-neg neg) y* err? err)))]))

(define (bigfloat-midpoint lo hi)
  (bfstep lo (inexact->exact (floor (/ (bigfloats-between lo hi) 2)))))

(define (convex-find-min fn xlo xhi)
  ; lgamma is always convex in the same direction
  (let loop ([lo xlo] [mlo (bfdiv (bfadd (bfadd xlo xhi) xlo) 3.bf)]
             [mhi (bfdiv (bfadd (bfadd xlo xhi) xhi) 3.bf)] [hi xhi])
    (let ([ylo (rnd 'up fn lo)] [ymlo (rnd 'down fn mlo)]
          [yhi (rnd 'up fn hi)] [ymhi (rnd 'down fn mhi)])
      ;; Invariant: ylo >= ymlo and yhi >= ymhi.
      ;; Base case: ylo and yhi = +inf.bf
      ;; Therefore lgamma decreasing from lo to mlo and increasing from mhi to hi
      (cond
       [(<= (bigfloats-between lo hi) 3)
        (define dy1 (rnd 'up bfsub ymlo ylo))
        (define dy2 (rnd 'up bfsub ymhi yhi))
        ; Overcorrect for possible deviation downward
        (define dy (rnd 'up bfdiv (bfmax2 dy1 dy2) 2.bf))
        (values mlo (rnd 'down bfadd ymlo dy))]
       [(<= (bigfloats-between mlo mhi) 1) ; Close enough to exit
        (loop (bfprev mlo) mlo mhi (bfnext mhi))]
       [(bfgt? ymlo ymhi) ; If true, lgamma decreasing from mlo to mhi
        (loop mlo mhi (bigfloat-midpoint mhi hi) hi)]
       [else
        (loop lo (bigfloat-midpoint lo mlo) mlo mhi)]))))

;; These both assume that xmin and ymin are not immovable (they are computed with rounding)
(define ((convex fn xmin ymin) i)
  (match-define (ival lo hi err? err) i)
  (cond
   [(bfgt? (endpoint-val lo) xmin) ; Purely increasing
    ((monotonic fn) i)]
   [(bflt? (endpoint-val hi) xmin) ; Purely decreasing
    ((comonotonic fn) i)]
   [else
    (ival-union
     (ival (endpoint ymin #f) (rnd 'up epfn fn lo) err? err)
     (ival (endpoint ymin #f) (rnd 'up epfn fn hi) err? err))]))

; Optimized version of `ival-lgamma-basin` for positive values, adds a cache
(define lgamma-pos-xmin #f)
(define lgamma-pos-ymin #f)
(define lgamma-pos-prec #f)

(define (ival-lgamma-pos x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (cond
   [(bfgte? xlo (bf 1.5)) ; Fast path, gamma is increasing here
    ((monotonic bflog-gamma) x)]
   [(and (bfgte? xlo 0.bf) (bflte? xhi (bf 1.4))) ; Another fast path
    ((comonotonic bflog-gamma) x)]
   [else
    ;; Gamma has a single minimum for positive inputs, which is about 1.46163
    ;; This computation is common enough that we cache it
    (unless (and lgamma-pos-prec (<= (bf-precision) lgamma-pos-prec))
      (define-values (xmin ymin) (convex-find-min bflog-gamma (bf 1.46163) (bf 1.46164)))
      (set! lgamma-pos-xmin xmin)
      (set! lgamma-pos-ymin ymin)
      (set! lgamma-pos-prec (bf-precision)))
    ((convex bflog-gamma lgamma-pos-xmin lgamma-pos-ymin) x)]))

;; Crude estimate, used only when other option is not available.
;;  Note that G(-n + f) = +- G(f) G(1-f) / G(n + 1 - f).
;; Thus, the min of G over [-n, -n + 1] is greater than
;;   A) the min of G(f) G(1-f) over [0, 1]
;;   B) divided by the max of G(n + 1 - f) over [0, 1]
;; (A) is easy because that function is symmetric over [0, 1];
;;   the min is at 1/2 with value G(1/2)^2 = pi
;; (B) is also easy because G is increasing on [n, n + 1]
;;   since n is positive, so the max is at n + 1
;; Hence, G over [-n, -n + 1] is greater than
;;   pi / G(n + 1);
;; Hence, logG over [-n, -n + 1] is greater than
;;   log pi - logG(n + 1)
(define (ival-lgamma-basin-bound imin)
  (define logpi (rnd 'down bflog (rnd 'down pi.bf)))
  (define logg (rnd 'up bflog-gamma (rnd 'up bfadd 1.bf (bfneg imin))))
  (define bound (rnd 'down bfsub logpi logg))
  (ival (endpoint bound #f) (endpoint +inf.bf #f) #f #f))

;; Here we assume that x is entirely negative
;; and does not cross any integer boundaries.
(define (ival-lgamma-basin x)
  (define imin (bffloor (ival-lo-val x)))
  (define imax (bfceiling (ival-hi-val x)))
  (if (< (bigfloats-between imin imax) 25) ; Value 25 is not verified
      (ival-then x (ival-lgamma-basin-bound imin)) ; Too close, cannot use convex
      (let-values ([(xmin ymin) (convex-find-min bflog-gamma imin imax)])
        ((convex bflog-gamma xmin ymin) x))))

(define (ival-lgamma x)
  ; The starred versions allow #f for an empty interval
  (define (ival-lo-val* x) (if x (ival-lo-val x) 0.bf))
  (define (ival-split* i x) (if i (ival-split i x) (values #f #f)))
  (define (ival-union* a b) (if (and a b) (ival-union a b) (or a b)))

  (define-values (xneg xpos) (ival-split x 0.bf))
  (define-values (xnegl xrest) (ival-split* xneg (bfceiling (ival-lo-val* xneg))))
  (define-values (xnegr xdrop) (ival-split* xrest (rnd 'up bfadd 1.bf (ival-lo-val* xrest))))

  (if (or xpos xnegl xnegr)
      (ival-union*
       (and xpos (ival-lgamma-pos xpos))
       (ival-union*
        (and xnegl (ival-lgamma-basin xnegl))
        (and xnegr (ival-lgamma-basin xnegr))))
      ;; This case only happens if xnegr = #f meaning lo = rnd[up](lo + 1) meaning lo = -inf
      (mk-big-ival -inf.bf +inf.bf)))

(define (exact-bffloor x)
  (parameterize ([bf-precision (bigfloat-precision x)])
    (bffloor x)))

(define (ival-tgamma x)
  (define logy (ival-lgamma x))
  (unless logy
    (error 'ival-lgamma "Invalid input to ival-lgamma: ~a" x))
  (define absy (ival-exp logy))
  (define lo (ival-lo-val x))
  (define hi (ival-hi-val x))
  (cond
   [(bfgte? lo 0.bf)
    absy]
   [(not (bf=? (exact-bffloor lo) (exact-bffloor hi)))
    (ival (endpoint -inf.bf (ival-lo-fixed? x))
          (endpoint +inf.bf (ival-hi-fixed? x))
          #t (ival-err x))]
   [(and (not (bfpositive? lo)) (bf=? lo hi) (bfinteger? lo))
    ival-illegal]
   [(bfeven? (exact-bffloor lo))
    absy]
   [else
    (ival-neg absy)]))

(define* ival-erf (monotonic bferf))
(define* ival-erfc (comonotonic bferfc))

(define (ival-cmp x y)
  (define can-< (epfn bflt? (ival-lo x) (ival-hi y)))
  (define must-< (epfn bflt? (ival-hi x) (ival-lo y)))
  (define can-> (epfn bfgt? (ival-hi x) (ival-lo y)))
  (define must-> (epfn bfgt? (ival-lo x) (ival-hi y)))
  (values can-< must-< can-> must->))

(define (ival-<2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m< c< (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-<=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn not c>) (epfn not m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m> c> (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn not c<) (epfn not m<) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-==2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn and-fn (epfn not c<) (epfn not c>))
        (epfn and-fn (epfn not m<) (epfn not m>))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define ((ival-comparator f name) . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)] [tail (cdr as)] [acc ival-true])
        (match tail
          ['() acc]
          [(cons next rest)
           (loop next rest (ival-and (f head next) acc))]))))

(define* ival-<  (ival-comparator ival-<2  'ival-<))
(define* ival-<= (ival-comparator ival-<=2 'ival-<=))
(define* ival->  (ival-comparator ival->2  'ival->))
(define* ival->= (ival-comparator ival->=2 'ival->=))
(define* ival-== (ival-comparator ival-==2 'ival-==))

(define (ival-!=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn or-fn m< m>) (epfn or-fn c< c>)
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-!= . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)] [tail (cdr as)])
        (if (null? tail)
            ival-true
            (ival-and
             (foldl ival-and ival-true (map (curry ival-!=2 head) tail))
             (loop (car tail) (cdr tail)))))))

(define (ival-error? x)
  (ival (endpoint (ival-err x) #f) (endpoint (ival-err? x) #f) #f #f))

(define (ival-assert c [msg #t])
  (ival (endpoint #t #t) (endpoint #t #t)
        (or (ival-err? c) (if (ival-lo-val c) #f msg))
        (or (ival-err c) (if (ival-hi-val c) #f msg))))

(define ival-maybe (ival (endpoint #f #t) (endpoint #t #t) #f #f))

(define (ival-then a . as)
  (ival (ival-lo (last (cons a as))) (ival-hi (last (cons a as)))
        (or (ival-err? a) (ormap ival-err? as))
        (or (ival-err a) (ormap ival-err as))))

(define* ival-identity (monotonic bfcopy))

(define (ival-if c x y)
  (cond
   [(ival-lo-val c) (ival-then c (ival-identity x))]
   [(not (ival-hi-val c)) (ival-then c (ival-identity y))]
   [else (ival-then c (ival-union x y))]))

(define (ival-fmin x y)
  (ival (rnd 'down endpoint-min2 (ival-lo x) (ival-lo y))
        (rnd 'up endpoint-min2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-fmax x y)
  (ival (rnd 'down endpoint-max2 (ival-lo x) (ival-lo y))
        (rnd 'up endpoint-max2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-copysign x y)
  (match-define (ival xlo xhi xerr? xerr) (ival-fabs x))
  (define can-zero
    (or (bfzero? (ival-lo-val y)) (bfzero? (ival-hi-val y))))
  ;; 0 is both positive and negative because we don't handle signed zero well
  (define can-neg (or (= (bigfloat-signbit (ival-lo-val y)) 1) can-zero))
  (define can-pos (or (= (bigfloat-signbit (ival-hi-val y)) 0) can-zero))
  (define err? (or (ival-err? y) xerr?))
  (define err (or (ival-err y) xerr))
  (match* (can-neg can-pos)
    [(#t #t) (ival (rnd 'down epfn bfneg xhi) (rnd 'up epfn bfcopy xhi) err? err)]
    [(#t #f) (ival (rnd 'down epfn bfneg xhi) (rnd 'up epfn bfneg xlo) err? err)]
    [(#f #t) (ival xlo xhi err? err)]
    [(#f #f)
     (unless (ival-err y)
       (error 'ival-copysign "Strange interval ~a" y))
     ival-illegal]))

(define (ival-fdim x y)
  (ival-fmax (ival-sub x y) (mk-ival 0.bf)))

(define (ival-sort ivs cmp)
  (define upper (sort (map ival-hi-val ivs) cmp))
  (define lower (sort (map ival-lo-val ivs) cmp))
  (define err? (ormap (lambda (iv) (ival-err? iv)) ivs))
  (define err (ormap (lambda (iv) (ival-err iv)) ivs))
  (define hi! (andmap (lambda (iv) (ival-hi-fixed? iv)) ivs))
  (define lo! (andmap (lambda (iv) (ival-lo-fixed? iv)) ivs))
  (for/list ([u upper] [l lower])
            (ival (endpoint l lo!) (endpoint u hi!) err? err)))


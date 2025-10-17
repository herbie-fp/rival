#lang racket/base

(require math/private/bigfloat/mpfr
         ffi/unsafe)

(define *rival-max-precision* (make-parameter 10000))

(provide -inf.bf
         -1.bf
         0.bf
         half.bf
         1.bf
         2.bf
         3.bf
         +inf.bf
         +nan.bf
         bf-return-exact?
         rnd
         *rival-max-precision*)

(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define -inf.bf (bf -inf.0))
(define -1.bf (bf -1))
(define 0.bf (bf 0))
(define half.bf (bf 0.5))
(define 1.bf (bf 1))
(define 2.bf (bf 2))
(define 3.bf (bf 3))
(define +inf.bf (bf +inf.0))
(define +nan.bf (bf +nan.0))

;; Some hairy code follows to access the MPFR "inexact" exception.
;; It assumes no one else cares about the flag, so it clobbers it.
(define mpfr_clear_inexflag (get-mpfr-fun 'mpfr_clear_inexflag (_fun -> _void)))
(define mpfr_get_inexflag (get-mpfr-fun 'mpfr_inexflag_p (_fun -> _int)))

(define (bf-return-exact? op args)
  (mpfr_clear_inexflag)
  (define out (apply op args))
  (define exact? (= (mpfr_get_inexflag) 0))
  (values out exact?))
;; End hairy code

(define mpfr-add!
  (get-mpfr-fun 'mpfr_add (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-sub!
  (get-mpfr-fun 'mpfr_sub (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-mul!
  (get-mpfr-fun 'mpfr_mul (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-div!
  (get-mpfr-fun 'mpfr_div (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-remainder!
  (get-mpfr-fun 'mpfr_remainder (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-hypot!
  (get-mpfr-fun 'mpfr_hypot (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-pow!
  (get-mpfr-fun 'mpfr_pow (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-log! (get-mpfr-fun 'mpfr_log (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-exp! (get-mpfr-fun 'mpfr_exp (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-expm1! (get-mpfr-fun 'mpfr_expm1 (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-sqrt! (get-mpfr-fun 'mpfr_sqrt (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-sin! (get-mpfr-fun 'mpfr_sin (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-cos! (get-mpfr-fun 'mpfr_cos (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-tan! (get-mpfr-fun 'mpfr_tan (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-cbrt! (get-mpfr-fun 'mpfr_cbrt (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-neg! (get-mpfr-fun 'mpfr_neg (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-abs! (get-mpfr-fun 'mpfr_abs (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-cmpabs (get-mpfr-fun 'mpfr_cmpabs (_fun _mpfr-pointer _mpfr-pointer -> _int)))

(define mpfr-asin! (get-mpfr-fun 'mpfr_asin (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-acos! (get-mpfr-fun 'mpfr_acos (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-atan! (get-mpfr-fun 'mpfr_atan (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-sinh! (get-mpfr-fun 'mpfr_sinh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-tanh! (get-mpfr-fun 'mpfr_tanh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-asinh! (get-mpfr-fun 'mpfr_asinh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-acosh! (get-mpfr-fun 'mpfr_acosh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-atanh! (get-mpfr-fun 'mpfr_atanh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-erf! (get-mpfr-fun 'mpfr_erf (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-erfc! (get-mpfr-fun 'mpfr_erfc (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-log2! (get-mpfr-fun 'mpfr_log2 (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-log10! (get-mpfr-fun 'mpfr_log10 (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-log1p! (get-mpfr-fun 'mpfr_log1p (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-exp2! (get-mpfr-fun 'mpfr_exp2 (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-rint! (get-mpfr-fun 'mpfr_rint (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-round! (get-mpfr-fun 'mpfr_round (_fun _mpfr-pointer _mpfr-pointer -> _int)))

(define mpfr-ceil! (get-mpfr-fun 'mpfr_ceil (_fun _mpfr-pointer _mpfr-pointer -> _int)))

(define mpfr-floor! (get-mpfr-fun 'mpfr_floor (_fun _mpfr-pointer _mpfr-pointer -> _int)))

(define mpfr-trunc! (get-mpfr-fun 'mpfr_trunc (_fun _mpfr-pointer _mpfr-pointer -> _int)))

(define mpfr-min!
  (get-mpfr-fun 'mpfr_min (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-max!
  (get-mpfr-fun 'mpfr_max (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-cosh! (get-mpfr-fun 'mpfr_cosh (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-const-pi! (get-mpfr-fun 'mpfr_const_pi (_fun _mpfr-pointer _rnd_t -> _int)))

(define mpfr-atan2!
  (get-mpfr-fun 'mpfr_atan2 (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-pow2! (get-mpfr-fun 'mpfr_sqr (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define mpfr-set! (get-mpfr-fun 'mpfr_set (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _void)))

(define mpfr-set-prec! set-mpfr-prec!)

(define (bfremainder x mod)
  (define out (bf 0))
  (mpfr-remainder! out x mod (bf-rounding-mode))
  out)

(define mpfr-fmod!
  (get-mpfr-fun 'mpfr_fmod (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define (bffmod x mod)
  (define out (bf 0))
  (mpfr-fmod! out x mod (bf-rounding-mode))
  out)

(define mpfr-log2p1! (get-mpfr-fun 'mpfr_log2p1 (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define (bflog2p1 x)
  (define out (bf 0))
  (mpfr-log2p1! out x (bf-rounding-mode))
  out)

(define mpfr-cosu!
  (get-mpfr-fun 'mpfr_cosu (_fun _mpfr-pointer _mpfr-pointer _ulong _rnd_t -> _int) (lambda () #f)))
(define mpfr-sinu!
  (get-mpfr-fun 'mpfr_sinu (_fun _mpfr-pointer _mpfr-pointer _ulong _rnd_t -> _int) (lambda () #f)))
(define mpfr-tanu!
  (get-mpfr-fun 'mpfr_tanu (_fun _mpfr-pointer _mpfr-pointer _ulong _rnd_t -> _int) (lambda () #f)))

(define (bfcosu n x)
  (define out (bf 0))
  (mpfr-cosu! out x n (bf-rounding-mode))
  out)

(define (bfsinu n x)
  (define out (bf 0))
  (mpfr-sinu! out x n (bf-rounding-mode))
  out)

(define (bftanu n x)
  (define out (bf 0))
  (mpfr-tanu! out x n (bf-rounding-mode))
  out)

(unless mpfr-cosu!
  (set! bfcosu #f))
(unless mpfr-sinu!
  (set! bfsinu #f))
(unless mpfr-tanu!
  (set! bftanu #f))

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define (bfcopysign x y)
  (if (bfnan? y)
      +nan.bf
      (bfmul (bfabs x) (if (= (bigfloat-signbit y) 1) -1.bf 1.bf))))

(define (bffdim x y)
  (if (bfgt? x y)
      (bfsub x y)
      0.bf))

(define (and-fn . as)
  (andmap values as))
(define (or-fn . as)
  (ormap values as))

(define (if-fn c x y)
  (if c x y))

(define (bffma a b c)
  ;; `bfstep` truncates to `(bf-precision)` bits
  (bfcopy (bfadd c
                 (parameterize ([bf-precision (* (bf-precision) 2)])
                   (bfmul a b)))))

(provide bf
         bigfloat?
         mpfr-sign
         bigfloat-exponent
         bigfloat-precision
         bf-precision
         mpfr-exp
         bf-rounding-mode
         bfpositive?
         bfinteger?
         bfzero?
         bfnan?
         bfinfinite?
         bfnegative?
         bfrational?
         bfeven?
         bfodd?
         bfcopy
         bfstep
         bigfloat->flonum
         bigfloats-between
         bfprev
         bfnext
         bf=?
         bflte?
         bfgte?
         bflt?
         bfgt?
         bfgte?
         pi.bf
         bfmin2
         bfmax2
         bfabs
         bfadd
         bfsub
         bfneg
         bfmul
         bfdiv
         bfremainder
         bfrint
         bfround
         bfceiling
         bffloor
         bftruncate
         bfexp
         bflog
         bfexp2
         bfexpm1
         bflog2
         bflog1p
         bflog10
         bfexpt
         bfsqrt
         bfcbrt
         bfhypot
         bfsin
         bfcos
         bftan
         bfsinh
         bfcosh
         bftanh
         bfcosu
         bfsinu
         bftanu
         bfasin
         bfacos
         bfatan
         bfatan2
         bfasinh
         bfacosh
         bfatanh
         bflog-gamma
         bfgamma
         bferf
         bferfc
         bfremainder
         bffmod
         bflogb
         bfcopysign
         bffdim
         and-fn
         or-fn
         if-fn
         bffma
         mpfr-add!
         mpfr-sub!
         mpfr-mul!
         mpfr-div!
         mpfr-hypot!
         mpfr-pow!
         mpfr-exp!
         mpfr-expm1!
         mpfr-sqrt!
         mpfr-sin!
         mpfr-cos!
         mpfr-tan!
         mpfr-cosu!
         mpfr-sinu!
         mpfr-tanu!
         mpfr-cbrt!
         mpfr-neg!
         mpfr-abs!
         mpfr-cmpabs
         mpfr-asin!
         mpfr-acos!
         mpfr-atan!
         mpfr-sinh!
         mpfr-cosh!
         mpfr-tanh!
         mpfr-asinh!
         mpfr-acosh!
         mpfr-atanh!
         mpfr-erf!
         mpfr-erfc!
         mpfr-log2!
         mpfr-log10!
         mpfr-log1p!
         mpfr-exp2!
         mpfr-rint!
         mpfr-round!
         mpfr-ceil!
         mpfr-floor!
         mpfr-trunc!
         mpfr-min!
         mpfr-max!
         mpfr-log!
         mpfr-const-pi!
         mpfr-atan2!
         mpfr-pow2!
         mpfr-set-prec!
         mpfr-set!)

#lang racket/base

(require math/private/bigfloat/mpfr ffi/unsafe)

(provide -inf.bf -1.bf 0.bf half.bf 1.bf 2.bf 3.bf +inf.bf +nan.bf
         bf-return-exact? rnd)

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

(provide bf bigfloat? bf=? bfnan? bfinfinite? pi.bf bfexp bflte? bfgte? mpfr-sign bfzero? bfmin2 bfmax2 bfadd bfsub bfmul bfdiv bflt? bfgt? bfgte? bfneg bigfloat-exponent bfrint bfround bfceiling bffloor bftruncate bfabs bigfloat-precision bf-precision bflog bfprev bfnext bfexp bfexp2 bfexpm1 bflog2 bflog1p bflog10 bfsqrt bfcbrt bfexpt bfhypot bfcos bfeven? bfodd? bfsin bftan bfcos bfsinh bfcosh bftanh bfasin bfacos bfatan bfatan2 bfasinh bfacosh bfatanh bfstep bigfloats-between bflog-gamma bfpositive? bfinteger? bferf bferfc bfcopy mpfr-exp)


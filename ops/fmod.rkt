#lang racket/base

(require "core.rkt"
         "../mpfr.rkt")
(provide ival-fmod
         ival-remainder)

(define (bfmul* a b)
  (if (or (bfzero? a) (bfzero? b))
      0.bf
      (bfmul a b)))

(define (ival-fmod-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define precision (max (bf-precision) (ival-max-prec x) (ival-max-prec y)))
  (define a
    (parameterize ([bf-precision precision])
      (rnd 'down bftruncate (bfdiv (ival-lo-val x) (ival-hi-val y)))))
  (define b
    (parameterize ([bf-precision precision])
      (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y)))))
  (cond
    [(bf=? a b) ; No intersection along `y.hi` edge
     (define c
       (parameterize ([bf-precision precision])
         (rnd 'down bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y)))))
     (define d
       (parameterize ([bf-precision precision])
         (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-lo-val y)))))
     (cond
       [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
        (define lo
          (rnd 'down
               bfsub
               (ival-lo-val x)
               (parameterize ([bf-precision precision])
                 (rnd 'up bfmul* c (ival-hi-val y)))))
        (define hi
          (rnd 'up
               bfsub
               (ival-hi-val x)
               (parameterize ([bf-precision precision])
                 (rnd 'down bfmul* c (ival-lo-val y)))))
        (ival (endpoint lo #f) (endpoint hi #f) err? err)]
       [else
        (ival (endpoint 0.bf #f)
              (endpoint (rnd 'up
                             bfmax2
                             (parameterize ([bf-precision precision])
                               (bfdiv (ival-hi-val x) (rnd 'down bfadd c 1.bf)))
                             0.bf)
                        #f)
              err?
              err)])]
    [else (ival (endpoint 0.bf #f) (endpoint (ival-hi-val y) #f) err? err)]))

(define (ival-fmod x y)
  (define err?
    (or (ival-err? x)
        (ival-err? y)
        (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err
    (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-exact-fabs y))
  (cond
    [(bflte? (ival-hi-val x) 0.bf) (ival-neg (ival-fmod-pos (ival-exact-neg x) y* err? err))]
    [(bfgte? (ival-lo-val x) 0.bf) (ival-fmod-pos x y* err? err)]
    [else
     (define-values (neg pos) (split-ival x 0.bf))
     (ival-union (ival-fmod-pos pos y* err? err)
                 (ival-neg (ival-fmod-pos (ival-exact-neg neg) y* err? err)))]))

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
        (define y* (rnd 'up bfdiv (ival-hi-val y) 2.bf))
        (ival
         (endpoint
          (rnd 'down bfmax2 (bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y))) (bfneg y*))
          #f)
         (endpoint (rnd 'up bfmin2 (bfsub (ival-hi-val x) (rnd 'down bfmul c (ival-lo-val y))) y*) #f)
         err?
         err)]
       [else
        ;; NOPE! need to subtract half.bf one way, add it another!
        (define y*-hi (rnd 'up bfdiv (bfdiv (ival-hi-val x) (rnd 'down bfadd c half.bf)) 2.bf))
        (define y*-lo
          (rnd 'down
               bfmax2
               (bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y)))
               (bfneg (bfdiv (ival-hi-val y) 2.bf))))
        (ival (endpoint (rnd 'down bfmin2 y*-lo (bfneg y*-hi)) #f) (endpoint y*-hi #f) err? err)])]
    [else
     (define y* (rnd 'up bfdiv (ival-hi-val y) 2.bf))
     (ival (endpoint (rnd 'down bfneg y*) #f) (endpoint y* #f) err? err)]))

;; Seems unnecessary
(define (ival-remainder x y)
  (define err?
    (or (ival-err? x)
        (ival-err? y)
        (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err
    (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-exact-fabs y))
  (cond
    [(bflte? (ival-hi-val x) 0.bf) (ival-neg (ival-remainder-pos (ival-exact-neg x) y* err? err))]
    [(bfgte? (ival-lo-val x) 0.bf) (ival-remainder-pos x y* err? err)]
    [else
     (define-values (neg pos) (split-ival x 0.bf))
     (ival-union (ival-remainder-pos pos y* err? err)
                 (ival-neg (ival-remainder-pos (ival-exact-neg neg) y* err? err)))]))

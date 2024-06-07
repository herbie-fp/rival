#lang racket

(require racket/math math/base math/flonum math/bigfloat racket/random)
(require "main.rkt" "test.rkt")

(define total-vals 1000)
(define sample-vals (make-parameter 2000))

(define (get-timings ival-fn itypes otype)
  (define n
    (if (set-member? slow-tests ival-fn)
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (define times (make-vector n))
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (sample-interval itype)))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (define dt (- (current-inexact-milliseconds) start))
    (vector-set! times i dt))
  (vector->list times))

(define (get-all-timings)
  (for/list ([fn (in-list function-table)])
    (match-define (list ival-fn bf-fn itypes otype) fn)
    (define t (get-timings ival-fn itypes otype))
    (define avg (/ (apply + t) (length t)))
    (define stdev (sqrt (/ (apply + (for/list ([v t]) (expt (- v avg) 2))) (- (length t) 1.5))))
    (define serr (/ stdev (sqrt (length t))))
    (list (object-name ival-fn) avg serr)))

(define (run)
  (for ([rec (in-list (get-all-timings))])
    (match-define (list name avg se) rec)
    (printf "~a [~a, ~a]µs\n"
            (~a name #:align 'left #:min-width 20)
            (~r (* (- avg se se) 1000)  #:precision '(= 3) #:min-width 8)
            (~r (* (+ avg se se) 1000)   #:precision '(= 3) #:min-width 8))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args ([n "1000"])
   (sample-vals (string->number n))
   (run)))

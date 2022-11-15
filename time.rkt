#lang racket

(require racket/math math/base math/flonum math/bigfloat racket/random)
(require "main.rkt" "test.rkt")

(define total-vals 1000)
(define sample-vals (make-parameter 1000))

(define (mk-initial-values n)
  (make-hash
   (list (cons 'real (build-list n (λ (i) (sample-interval 'real))))
         (cons 'bool (list (ival #t) (ival #f) (ival #f #t))))))

(define (exec-fn vals ival-fn itypes otype)
  (define args
    (for/list ([itype (in-list itypes)])
      (random-ref (hash-ref vals itype))))
  (define out (apply ival-fn args))
  (if (ival-valid? out)
      (hash-update! vals otype (curry cons out))
      (printf "Invalid output ~a from (~a ~a)\n"
              out (object-name ival-fn)
              (string-join (map ~a args) " "))))

(define (exec-real-fn vals)
  (match-define (list ival-fn bf-fn itypes otype)
                (random-ref function-table))
  (if (eq? otype 'real)
      (exec-fn vals ival-fn itypes otype)
      (exec-real-fn vals)))

(define (mk-values)
  (define vals (mk-initial-values 25))
  (for ([n (in-range 25 total-vals)])
    (exec-real-fn vals))
  vals)

(define (get-timings vals ival-fn itypes otype)
  (define tot '())
  (define n
    (if (string-contains? (~a (object-name ival-fn)) "gamma")
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (random-ref (hash-ref vals itype))))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (set! tot (cons (- (current-inexact-milliseconds) start) tot)))
  tot)

(define (get-all-timings)
  (define vals (mk-values))
  (for/list ([fn (in-list function-table)])
    (match-define (list ival-fn bf-fn itypes otype) fn)
    (define t (get-timings vals ival-fn itypes otype))
    (define avg (/ (apply + t) (length t)))
    (define stdev (sqrt (/ (apply + (for/list ([v t]) (expt (- v avg) 2))) (- (length t) 1.5))))
    (define serr (/ stdev (sqrt (length t))))
    (list (object-name ival-fn) avg serr)))

(define (run)
  (for ([rec (in-list (get-all-timings))])
    (match-define (list name avg se) rec)
    (printf "~a ~aµs ± ~a\n"
            (~a name #:align 'left #:min-width 20)
            (~r (* avg 1000)  #:precision '(= 3) #:min-width 8)
            (~r (* se 1000)   #:precision '(= 3) #:min-width 8))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args ([n "1000"])
   (sample-vals (string->number n))
   (run)))

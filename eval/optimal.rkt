#lang racket

(require "../ops/all.rkt"
         "machine.rkt"
         "run.rkt"
         "main.rkt")

(provide rival-machine-test-precision
         rival-machine-search-precision
         rival-machine-find-optimal-precisions)

; Test if a machine succeeds at a given point with a specific precision vector
; Returns #t if the evaluation succeeds (good? and done?), #f otherwise
(define (rival-machine-test-precision machine pt prec-vec)
  ; Load point into registers
  (define ival-pt
    (for/vector #:length (vector-length pt)
                ([x (in-vector pt)])
      (ival x)))
  (rival-machine-load machine ival-pt)

  ; Set custom precision vector
  (set-rival-machine-iteration! machine 1) ; Don't use initial precision vector
  (vector-copy! (rival-machine-precisions machine) 0 prec-vec)
  (vector-copy! (rival-machine-repeats machine) 0 (rival-machine-initial-repeats machine))
  (rival-machine-run machine (rival-machine-default-hint machine))

  ; Check result
  (define-values (good? done? bad? stuck? fvec) (rival-machine-return machine))
  (and good? done?))

; Binary search for the lowest precision at index idx that makes the machine succeed
; Returns the minimum precision in [min-prec, max-prec] where evaluation succeeds,
; or #f if even max-prec fails
(define (rival-machine-search-precision machine pt prec-vec idx)
  (define test-vec (vector-copy prec-vec))
  (define max-prec (vector-ref test-vec idx))

  ; Check if max-prec works at all
  (unless (rival-machine-test-precision machine pt test-vec)
    (error 'rival-machine-search-precision "max-prec does not succeed"))

  ; Binary search for minimum
  (let loop ([lo 2]
             [hi max-prec])
    (if (>= lo hi)
        hi
        (let* ([mid (quotient (+ lo hi) 2)])
          (vector-set! test-vec idx mid)
          (if (rival-machine-test-precision machine pt test-vec)
              (loop lo mid)
              (loop (+ mid 1) hi))))))

; Run thunk n times and return the minimum time
(define (time-min thunk #:min [n 5] #:sum [m 10])
  (thunk) ; Discard warm-up run
  (apply min
         (for/list ([i (in-range n)])
           (define start (current-inexact-milliseconds))
           (for ([i (in-range m)])
             (thunk))
           (/ (- (current-inexact-milliseconds) start) m))))

; Find optimal precisions for a machine at a given point
(define (rival-machine-find-optimal-precisions machine pt)
  ; Extract the precision assignment, assuming no slack
  (define out (rival-apply machine pt))
  (set-rival-machine-iteration! machine 1) ; Don't use initial precision vector
  (rival-machine-adjust machine (rival-machine-default-hint machine))
  (define max-precs (vector-copy (rival-machine-precisions machine)))

  (cond
    [(rival-machine-test-precision machine pt max-precs)
     ; Timed run with rival-apply (full evaluation), take min of 5 runs
     (define final-time (time-min (lambda () (rival-machine-test-precision machine pt max-precs))))

     ; Start with max precisions
     (define optimal-precs (vector-copy max-precs))
     (define n-instrs (vector-length (rival-machine-instructions machine)))
     (for ([idx (in-range (- n-instrs 1) -1 -1)])
       (vector-set! optimal-precs idx (rival-machine-search-precision machine pt optimal-precs idx)))

     ; Time run with optimal precisions
     (define optimal-time (time-min (lambda () (rival-machine-test-precision machine pt optimal-precs))))

     ; Return both ratios and precision vectors
     (list optimal-precs optimal-time max-precs final-time)]
    [else
     #f]))

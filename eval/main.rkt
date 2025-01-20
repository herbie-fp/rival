#lang racket

(require racket/flonum)
(require "../ops/all.rkt"
         "machine.rkt"
         "compile.rkt"
         "run.rkt"
         "adjust.rkt")

(provide rival-compile
         rival-apply
         rival-analyze
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         (struct-out discretization)
         *rival-max-precision*
         *rival-max-iterations*
         *rival-use-shorthands*
         *rival-name-constants*
         rival-profile
         (struct-out execution)
         *rival-profile-executions*)

(define ground-truth-require-convergence (make-parameter #t))

(define (rival-machine-full machine inputs [vhint (rival-machine-hint machine)])
  (set-rival-machine-iteration! machine (*sampling-iteration*))
  (rival-machine-adjust machine vhint)
  (cond
    [(>= (*sampling-iteration*) (*rival-max-iterations*)) (values #f #f #f #t #f)]
    [else
     (rival-machine-load machine inputs)
     (rival-machine-run machine vhint)
     (rival-machine-return machine)]))

(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt))
(struct exn:rival:unsamplable exn:rival (pt))

(struct execution (name number precision time) #:prefab)

(define (rival-profile machine param)
  (match param
    ['instructions (vector-length (rival-machine-instructions machine))]
    ['iterations (rival-machine-iteration machine)]
    ['bumps (rival-machine-bumps machine)]
    ['executions
     (define profile-ptr (rival-machine-profile-ptr machine))
     (define profile-instruction (rival-machine-profile-instruction machine))
     (define profile-number (rival-machine-profile-number machine))
     (define profile-time (rival-machine-profile-time machine))
     (define profile-precision (rival-machine-profile-precision machine))
     (begin0 (for/vector #:length profile-ptr
                         ([instruction (in-vector profile-instruction 0 profile-ptr)]
                          [number (in-vector profile-number 0 profile-ptr)]
                          [precision (in-vector profile-precision 0 profile-ptr)]
                          [time (in-flvector profile-time 0 profile-ptr)])
               (execution instruction number precision time))
       (set-rival-machine-profile-ptr! machine 0))]))

(define (ival-real x)
  (ival x))

(define (rival-apply machine pt [hint (rival-machine-hint machine)])
  (define discs (rival-machine-discs machine))
  (set-rival-machine-bumps! machine 0)
  (let loop ([iter 0])
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([*sampling-iteration* iter]
                     [ground-truth-require-convergence #t])
        (rival-machine-full machine (vector-map ival-real pt) hint)))
    (cond
      [bad? (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done? fvec]
      [stuck? (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(>= iter (*rival-max-iterations*))
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else (loop (+ 1 iter))])))

(define (rival-analyze machine rect)
  (define-values (good? done? bad? stuck? fvec)
    (parameterize ([*sampling-iteration* 0]
                   [ground-truth-require-convergence #f])
      (rival-machine-full machine rect)))
  (define-values (hint hint-converged?) (make-hint machine))
  (list (ival (or bad? stuck?) (not good?)) hint hint-converged?))

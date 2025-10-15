#lang racket/base

(require racket
         math/bigfloat
         math/flonum)

(require (only-in "../eval/compile.rkt" exprs->batch fn->ival-fn make-initial-repeats)
         (only-in "../eval/machine.rkt" *rival-max-precision* *rival-profile-executions*)
         (only-in "../eval/run.rkt"
                  rival-machine-load
                  rival-machine-run
                  rival-machine-return
                  rival-machine-record)
         (only-in "../eval/adjust.rkt" drop-self-pointer make-hint)
         "../eval/machine.rkt"
         "../eval/main.rkt"
         (only-in "../ops/core.rkt" new-ival)
         "../ops/all.rkt")

(provide baseline-compile
         baseline-analyze
         baseline-apply)

; ----------------------------------------- COMPILATION ----------------------------------------------
(define (baseline-compile exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots) (exprs->batch exprs vars)) ; translations are taken from Rival machine
  (define register-count (vector-length nodes))
  (define registers (make-vector register-count))
  (define instructions
    (for/vector #:length (- register-count num-vars)
                ([node (in-vector nodes num-vars)]
                 [n (in-naturals num-vars)])
      (fn->ival-fn node ; mappings are taken from Rival machine
                   (lambda ()
                     (vector-set! registers n (new-ival (*rival-max-precision*)))
                     n))))

  (define start-prec (+ (discretization-target (last discs)) 10))
  (define precisions
    (make-vector (- register-count num-vars) start-prec)) ; vector that stores working precisions
  (define best-known-precisions (make-vector (- register-count num-vars) 0)) ; for constant ops
  (define initial-precisions (make-vector (- register-count num-vars) start-prec))

  (define repeats (make-vector (- register-count num-vars)))
  (define initial-repeats
    (make-initial-repeats instructions num-vars registers initial-precisions best-known-precisions))

  (define default-hint (make-vector (- register-count num-vars) #t))

  (rival-machine (list->vector vars)
                 instructions
                 roots
                 (list->vector discs)
                 registers
                 repeats
                 initial-repeats
                 precisions
                 initial-precisions
                 best-known-precisions
                 (make-vector (vector-length roots)) ; output-distance
                 default-hint
                 (make-vector (- register-count num-vars) '()) ; constant-lookup
                 (*rival-max-precision*)
                 0 ; iteration
                 0 ; bumps
                 0 ; profile-ptr
                 (make-vector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))
                 (make-flvector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))
                 (make-vector (*rival-profile-executions*))))

; ------------------------------------------- APPLY --------------------------------------------------
(define (ival-real x)
  (ival x))

(define (baseline-apply machine pt [hint #f])
  (define discs (rival-machine-discs machine))
  (define max-precision (rival-machine-max-precision machine))
  (define start-prec (+ (discretization-target (vector-ref discs (- (vector-length discs) 1))) 10))
  ; Load arguments
  (rival-machine-load machine (vector-map ival-real pt))
  (let loop ([prec start-prec]
             [iter 0])
    (set-rival-machine-iteration! machine iter)
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([bf-precision prec])
        (baseline-machine-full machine (or hint (rival-machine-default-hint machine)))))
    (cond
      [bad? (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done? fvec]
      [stuck? (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(> (* 2 prec) max-precision) ; max precision is taken from eval/machine.rkt
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else (loop (* 2 prec) (+ iter 1))])))

(define (baseline-analyze machine rect [hint #f])
  (rival-machine-load machine rect)
  (set-rival-machine-iteration! machine 0)
  (define-values (good? done? bad? stuck? fvec)
    (baseline-machine-full machine (or hint (rival-machine-default-hint machine))))
  (define-values (hint* hint*-converged?)
    (make-hint machine (or hint (rival-machine-default-hint machine))))
  (list (ival (or bad? stuck?) (not good?)) hint* hint*-converged?))

(define (baseline-machine-adjust machine)
  (let ([start-time (current-inexact-milliseconds)]
        [start-memory (current-memory-use 'cumulative)])
    (define new-prec (bf-precision))
    (vector-fill! (rival-machine-precisions machine) new-prec)

    ; Whether a register is fixed already
    (define iter (rival-machine-iteration machine))
    (unless (zero? iter)
      (define ivec (rival-machine-instructions machine))
      (define vregs (rival-machine-registers machine))
      (define rootvec (rival-machine-outputs machine))
      (define vrepeats (rival-machine-repeats machine))
      (define args (rival-machine-arguments machine))
      (define vbest-precs (rival-machine-best-known-precisions machine))
      (define vinitial-repeats (rival-machine-initial-repeats machine))
      (define varc (vector-length args))
      (define vuseful (make-vector (vector-length ivec) #f))

      ; Useful feature
      (for ([root (in-vector rootvec)]
            #:when (>= root varc))
        (vector-set! vuseful (- root varc) #t))
      (for ([reg (in-vector vregs (- (vector-length vregs) 1) (- varc 1) -1)]
            [instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
            [i (in-range (- (vector-length ivec) 1) -1 -1)]
            [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)])
        (cond
          [(and (ival-lo-fixed? reg) (ival-hi-fixed? reg)) (vector-set! vuseful i #f)]
          [useful?
           (for ([arg (in-list (drop-self-pointer (cdr instr) (+ i varc)))]
                 #:when (>= arg varc))
             (vector-set! vuseful (- arg varc) #t))]))

      ; Constant operations
      (for ([instr (in-vector ivec)]
            [useful? (in-vector vuseful)]
            [best-known-precision (in-vector vbest-precs)]
            [constant? (in-vector vinitial-repeats)]
            [n (in-naturals)])
        (define tail-registers (drop-self-pointer (cdr instr) (+ n varc)))
        ; When instr is a constant instruction - keep tracks of old precision with vbest-precs vector
        (define no-need-to-reevaluate
          (and constant?
               (<= new-prec best-known-precision)
               (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc)))) tail-registers)))
        (define result-is-exact-already (not useful?))
        (define repeat (or result-is-exact-already no-need-to-reevaluate))

        ; Precision of const instruction has increased + it will be reexecuted under that precision
        (when (and constant? (not repeat) (not no-need-to-reevaluate))
          (vector-set! vbest-precs
                       n
                       new-prec)) ; record new best precision for the constant instruction
        (vector-set! vrepeats n repeat)))

    (rival-machine-record machine
                          'adjust
                          -1
                          (* iter 1000)
                          (- (current-inexact-milliseconds) start-time)
                          (- (current-memory-use 'cumulative) start-memory)
                          iter)))

(define (baseline-machine-full machine vhint)
  (baseline-machine-adjust machine)
  (rival-machine-run machine vhint)
  (rival-machine-return machine))

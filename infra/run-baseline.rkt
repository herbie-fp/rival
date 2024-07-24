#lang racket/base

(require racket math/bigfloat)
 
(require (only-in "../eval/compile.rkt" exprs->batch fn->ival-fn)
         (only-in "../eval/machine.rkt" *base-tuning-precision* *rival-max-precision*)
         "../eval/main.rkt"
         "../ops/all.rkt")

(provide baseline-compile baseline-apply)

(struct baseline-machine
  (arguments instructions outputs discs
             registers))

; ----------------------------------------- COMPILATION ----------------------------------------------
(define (baseline-compile exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots)
    (exprs->batch exprs vars)) ; translations are taken from Rival machine

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
      ([node (in-vector nodes num-vars)])
      (fn->ival-fn node)))     ; mappings are taken from Rival machine

  (define register-count (+ (length vars) (vector-length instructions)))
  (define registers (make-vector register-count))
  (define precisions (make-vector register-count)) ; vector that stores working precisions

  (baseline-machine
   (list->vector vars) instructions roots discs
   registers))

; ------------------------------------------- APPLY --------------------------------------------------
(define (ival-real x)
  (ival x))

(define (baseline-apply machine pt #:timeout [timeout 20.0])
  (define start-time (current-inexact-milliseconds))
  (define discs (baseline-machine-discs machine))
  (define start-prec (+ (discretization-target (last discs))
                        (*base-tuning-precision*))) ; base tuning is taken from eval/machine.rkt
  
  (let loop ([prec start-prec])
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([bf-precision prec])
        (baseline-machine-full machine (vector-map ival-real pt))))
    (cond
      [bad?
       (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done?
       fvec]
      [stuck?
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(or (>= (* 2 prec) (*rival-max-precision*)) ; max precision is taken from eval/machine.rkt
           (> (- (current-inexact-milliseconds) start-time) (+ timeout 5.0)))
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else
       (loop (* 2 prec))])))

(define (baseline-machine-full machine inputs)
  ; no adjust here
  (baseline-machine-load machine inputs)
  (baseline-machine-run machine)
  (baseline-machine-return machine))

(define (baseline-machine-load machine args)
  (vector-copy! (baseline-machine-registers machine) 0 args))

(define (baseline-machine-run machine)
  (define ivec (baseline-machine-instructions machine))
  (define varc (vector-length (baseline-machine-arguments machine)))
  (define vregs (baseline-machine-registers machine))

  (parameterize ([bf-precision (bf-precision)])
    (for ([instr (in-vector ivec)]
          [n (in-naturals varc)])
      (vector-set! vregs n (apply-instruction instr vregs)))))

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a)
     (op (vector-ref regs a))]
    [(list op a b)
     (op (vector-ref regs a)
         (vector-ref regs b))]
    [(list op a b c)
     (op (vector-ref regs a)
         (vector-ref regs b)
         (vector-ref regs c))]
    [(list op args ...)
     (apply op (map (curryr vector-ref regs) args))]))

(define (baseline-machine-return machine)
  (define discs (baseline-machine-discs machine))
  (define vregs (baseline-machine-registers machine))
  (define rootvec (baseline-machine-outputs machine))
  (define ovec (make-vector (vector-length rootvec)))
  (define good? #t)
  (define done? #t)
  (define bad? #f)
  (define stuck? #f)
  (define fvec
    (for/vector #:length (vector-length rootvec)
                ([root (in-vector rootvec)] [disc (in-list discs)] [n (in-naturals)])
      (define out (vector-ref vregs root))
      (define lo ((discretization-convert disc) (ival-lo out)))
      (define hi ((discretization-convert disc) (ival-hi out)))
      (define distance ((discretization-distance disc) lo hi))
      (unless (= distance 0)
        (set! done? #f)
        (when (and (ival-lo-fixed? out) (ival-hi-fixed? out))
          (set! stuck? #t)))
      (cond
        [(ival-err out) (set! bad? #t)]
        [(ival-err? out) (set! good? #f)])
      lo))
  (values good? (and good? done?) bad? stuck? fvec))
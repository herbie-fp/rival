#lang racket/base

(require racket
         math/bigfloat
         math/flonum)

(require (only-in "../eval/compile.rkt" exprs->batch fn->ival-fn)
         (only-in "../eval/machine.rkt"
                  *base-tuning-precision*
                  *rival-max-precision*
                  *rival-profile-executions*)
         "../eval/main.rkt"
         "../ops/all.rkt")

(provide baseline-compile
         baseline-apply
         baseline-profile
         (struct-out baseline-machine))

(struct baseline-machine
        (arguments instructions
                   outputs
                   discs
                   registers
                   precisions
                   repeats
                   default-hint
                   [iteration #:mutable]
                   [precision #:mutable]
                   [profile-ptr #:mutable]
                   profile-instruction
                   profile-number
                   profile-time
                   profile-precision))

; ----------------------------------------- COMPILATION ----------------------------------------------
(define (baseline-compile exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots) (exprs->batch exprs vars)) ; translations are taken from Rival machine

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
                ([node (in-vector nodes num-vars)])
      (fn->ival-fn node))) ; mappings are taken from Rival machine

  (define register-count (+ (length vars) (vector-length instructions)))
  (define registers (make-vector register-count))
  (define precisions
    (make-vector (vector-length instructions))) ; vector that stores working precisions
  (define repeats (make-vector (vector-length instructions)))
  (define hint (make-vector (vector-length instructions) #t))

  (baseline-machine (list->vector vars)
                    instructions
                    roots
                    discs
                    registers
                    precisions
                    repeats
                    hint
                    0
                    0
                    0
                    (make-vector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))
                    (make-flvector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))))

; ------------------------------------------- APPLY --------------------------------------------------
(define (ival-real x)
  (ival x))

(define (baseline-apply machine pt [hint #f])
  (define discs (baseline-machine-discs machine))
  (define start-prec
    (+ (discretization-target (last discs))
       (*base-tuning-precision*))) ; base tuning is taken from eval/machine.rkt
  ; Load arguments
  (baseline-machine-load machine (vector-map ival-real pt))
  (let loop ([prec start-prec]
             [iter 0])
    (set-baseline-machine-iteration! machine iter)
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([bf-precision prec])
        (baseline-machine-full machine (or hint (baseline-machine-default-hint machine)))))
    (cond
      [bad? (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done? fvec]
      [stuck? (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(> (* 2 prec) (*rival-max-precision*)) ; max precision is taken from eval/machine.rkt
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else (loop (* 2 prec) (+ iter 1))])))

(define (baseline-machine-adjust machine)
  (set-baseline-machine-precision! machine (bf-precision))
  (vector-fill! (baseline-machine-precisions machine) (bf-precision))

  ; Whether a register is fixed already
  (define iter (baseline-machine-iteration machine))
  (unless (zero? iter)
    (define ivec (baseline-machine-instructions machine))
    (define vregs (baseline-machine-registers machine))
    (define rootvec (baseline-machine-outputs machine))
    (define repeats (baseline-machine-repeats machine))
    (define args (baseline-machine-arguments machine))
    (define varc (vector-length args))

    (define vuseful (make-vector (vector-length ivec) #f))

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
         (for ([arg (in-list (cdr instr))]
               #:when (>= arg varc))
           (vector-set! vuseful (- arg varc) #t))]))
    (vector-copy! repeats 0 (vector-map not vuseful))))

(define (baseline-machine-full machine vhint)
  (baseline-machine-adjust machine)
  (baseline-machine-run machine vhint)
  (baseline-machine-return machine))

(define (baseline-machine-load machine args)
  (vector-copy! (baseline-machine-registers machine) 0 args))

(define (baseline-machine-run machine vhint)
  (define ivec (baseline-machine-instructions machine))
  (define varc (vector-length (baseline-machine-arguments machine)))
  (define vregs (baseline-machine-registers machine))
  (define precisions (baseline-machine-precisions machine))
  (define repeats (baseline-machine-repeats machine))
  (define first-iter? (zero? (baseline-machine-iteration machine)))

  (for ([instr (in-vector ivec)]
        [n (in-naturals varc)]
        [precision (in-vector precisions)]
        [repeat (in-vector repeats)]
        [hint (in-vector vhint)]
        #:unless (or (not hint) (and (not first-iter?) repeat)))
    (define start (current-inexact-milliseconds))
    (parameterize ([bf-precision precision])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (define name (object-name (car instr)))
    (define time (- (current-inexact-milliseconds) start))
    (baseline-machine-record machine name n precision time)))

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a) (op (vector-ref regs a))]
    [(list op a b) (op (vector-ref regs a) (vector-ref regs b))]
    [(list op a b c) (op (vector-ref regs a) (vector-ref regs b) (vector-ref regs c))]
    [(list op args ...) (apply op (map (curryr vector-ref regs) args))]))

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
                ([root (in-vector rootvec)]
                 [disc (in-list discs)]
                 [n (in-naturals)])
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

; ---------------------------------------- PROFILING -------------------------------------------------
(define (baseline-profile machine param)
  (match param
    ['instructions (vector-length (baseline-machine-instructions machine))]
    ['executions
     (define profile-ptr (baseline-machine-profile-ptr machine))
     (define profile-instruction (baseline-machine-profile-instruction machine))
     (define profile-number (baseline-machine-profile-number machine))
     (define profile-time (baseline-machine-profile-time machine))
     (define profile-precision (baseline-machine-profile-precision machine))
     (begin0 (for/vector #:length profile-ptr
                         ([instruction (in-vector profile-instruction 0 profile-ptr)]
                          [number (in-vector profile-number 0 profile-ptr)]
                          [precision (in-vector profile-precision 0 profile-ptr)]
                          [time (in-flvector profile-time 0 profile-ptr)])
               (execution instruction number precision time))
       (set-baseline-machine-profile-ptr! machine 0))]))

(define (baseline-machine-record machine name number precision time)
  (define profile-ptr (baseline-machine-profile-ptr machine))
  (define profile-instruction (baseline-machine-profile-instruction machine))
  (when (< profile-ptr (vector-length profile-instruction))
    (define profile-number (baseline-machine-profile-number machine))
    (define profile-time (baseline-machine-profile-time machine))
    (define profile-precision (baseline-machine-profile-precision machine))
    (vector-set! profile-instruction profile-ptr name)
    (vector-set! profile-number profile-ptr number)
    (vector-set! profile-precision profile-ptr precision)
    (flvector-set! profile-time profile-ptr time)
    (set-baseline-machine-profile-ptr! machine (add1 profile-ptr))))

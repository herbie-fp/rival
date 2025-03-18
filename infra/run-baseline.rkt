#lang racket/base

(require racket
         math/bigfloat
         math/flonum)

(require (only-in "../eval/compile.rkt" exprs->batch fn->ival-fn make-default-hint)
         (only-in "../eval/machine.rkt"
                  *base-tuning-precision*
                  *rival-max-precision*
                  *rival-profile-executions*)
         "../eval/main.rkt"
         "../ops/all.rkt")

(provide baseline-compile
         baseline-analyze
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

(define (make-hint machine old-hint)
  (define args (baseline-machine-arguments machine))
  (define ivec (baseline-machine-instructions machine))
  (define rootvec (baseline-machine-outputs machine))
  (define vregs (baseline-machine-registers machine))

  (define varc (vector-length args))
  (define vhint (make-vector (vector-length ivec) #f))
  (define converged? #t)

  ; helper function
  (define (vhint-set! idx val)
    (when (>= idx varc)
      (vector-set! vhint (- idx varc) val)))

  ; roots always should be executed
  (for ([root-reg (in-vector rootvec)])
    (vhint-set! root-reg #t))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [hint (in-vector vhint (- (vector-length vhint) 1) -1 -1)]
        [o-hint (in-vector old-hint (- (vector-length old-hint) 1) -1 -1)]
        [n (in-range (- (vector-length vhint) 1) -1 -1)]
        #:when hint)
    (define hint*
      (match o-hint
        [(? ival? _) o-hint] ; instr is already "hinted" by old hint, no children are to be recomputed
        [(? integer? ref) ; instr is already "hinted" by old hint,
         (define idx (list-ref instr ref)) ; however, one child needs to be recomputed
         (when (>= idx varc)
           (vhint-set! idx #t))
         o-hint]
        [#t
         (case (object-name (car instr))
           [(ival-assert)
            (match-define (list _ bool-idx) instr)
            (define bool-reg (vector-ref vregs bool-idx))
            (match* ((ival-lo bool-reg) (ival-hi bool-reg) (ival-err? bool-reg))
              ; assert and its children should not be reexecuted if it is true already
              [(#t #t #f) (ival-bool #t)]
              ; assert and its children should not be reexecuted if it is false already
              [(#f #f #f) (ival-bool #f)]
              [(_ _ _) ; assert and its children should be reexecuted
               (vhint-set! bool-idx #t)
               (set! converged? #f)
               #t])]
           [(ival-if)
            (match-define (list _ cond tru fls) instr)
            (define cond-reg (vector-ref vregs cond))
            (match* ((ival-lo cond-reg) (ival-hi cond-reg) (ival-err? cond-reg))
              [(#t #t #f) ; only true path should be executed
               (vhint-set! tru #t)
               2]
              [(#f #f #f) ; only false path should be executed
               (vhint-set! fls #t)
               3]
              [(_ _ _) ; execute both paths and cond as well
               (vhint-set! cond #t)
               (vhint-set! tru #t)
               (vhint-set! fls #t)
               (set! converged? #f)
               #t])]
           [(ival-fmax)
            (match-define (list _ arg1 arg2) instr)
            (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              [(#t #t #f) ; only arg1 should be executed
               (vhint-set! arg1 #t)
               1]
              [(#f #f #f) ; only arg2 should be executed
               (vhint-set! arg2 #t)
               2]
              [(_ _ _) ; both paths should be executed
               (vhint-set! arg1 #t)
               (vhint-set! arg2 #t)
               (set! converged? #f)
               #t])]
           [(ival-fmin)
            (match-define (list _ arg1 arg2) instr)
            (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              [(#t #t #f) ; only arg2 should be executed
               (vhint-set! arg2 #t)
               2]
              [(#f #f #f) ; only arg1 should be executed
               (vhint-set! arg1 #t)
               1]
              [(_ _ _) ; both paths should be executed
               (vhint-set! arg1 #t)
               (vhint-set! arg2 #t)
               (set! converged? #f)
               #t])]
           [(ival-< ival-<= ival-> ival->= ival-== ival-!= ival-and ival-or ival-not)
            (define cmp (vector-ref vregs (+ varc n)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              ; result is known
              [(#t #t #f) (ival-bool #t)]
              ; result is known
              [(#f #f #f) (ival-bool #f)]
              [(_ _ _) ; all the paths should be executed
               (define srcs (rest instr))
               (for-each (λ (x) (vhint-set! x #t)) srcs)
               (set! converged? #f)
               #t])]
           [else ; at this point we are given that the current instruction should be executed
            (define srcs (rest instr)) ; then, children instructions should be executed as well
            (for-each (λ (x) (vhint-set! x #t)) srcs)
            #t])]))
    (vector-set! vhint n hint*))
  (values vhint converged?))

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
  (define start-prec (+ (discretization-target (last discs)) (*base-tuning-precision*)))
  (define precisions
    (make-vector (vector-length instructions) start-prec)) ; vector that stores working precisions
  (define repeats (make-vector (vector-length instructions)))
  (define default-hint (make-default-hint instructions num-vars registers precisions))

  (baseline-machine (list->vector vars)
                    instructions
                    roots
                    discs
                    registers
                    precisions
                    repeats
                    default-hint
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

(define (baseline-analyze machine rect [hint #f])
  (baseline-machine-load machine rect)
  (set-baseline-machine-iteration! machine 0)
  (define-values (good? done? bad? stuck? fvec)
    (baseline-machine-full machine (or hint (baseline-machine-default-hint machine))))
  (define-values (hint* hint*-converged?)
    (make-hint machine (or hint (baseline-machine-default-hint machine))))
  (list (ival (or bad? stuck?) (not good?)) hint* hint*-converged?))

(define (baseline-machine-adjust machine)
  (let ([start (current-inexact-milliseconds)])
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
      (vector-copy! repeats 0 (vector-map not vuseful)))
    (baseline-machine-record machine
                             'adjust
                             -1
                             (* iter 1000)
                             (- (current-inexact-milliseconds) start))))

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
    (define out
      (match hint
        [#t
         (define start (current-inexact-milliseconds))
         (define res
           (parameterize ([bf-precision precision])
             (apply-instruction instr vregs)))
         (define name (object-name (car instr)))
         (define time (- (current-inexact-milliseconds) start))
         (baseline-machine-record machine name n precision time)
         res]
        [(box old-precision)
         (match (> precision old-precision)
           [#t ; reevaluate instruction at higher precision
            (define start (current-inexact-milliseconds))
            (define res
              (parameterize ([bf-precision precision])
                (apply-instruction instr vregs)))
            (define name (object-name (car instr)))
            (define time (- (current-inexact-milliseconds) start))
            (set-box! hint precision)
            (baseline-machine-record machine name n precision time)
            res]
           [#f (vector-ref vregs n)])]
        [(? integer? _) (vector-ref vregs (list-ref instr hint))]
        [(? ival? _) hint]))
    (vector-set! vregs n out)))

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
    ['iteration (baseline-machine-iteration machine)]
    ['precision (baseline-machine-precision machine)]
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

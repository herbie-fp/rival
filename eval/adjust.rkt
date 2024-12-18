#lang racket/base

(require "tricks.rkt"
         "../ops/all.rkt"
         "machine.rkt"
         racket/list
         racket/match)

(provide backward-pass
         make-hint)

(define (make-hint machine)
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define vregs (rival-machine-registers machine))

  (define varc (vector-length args))
  (define vhint (make-vector (vector-length ivec) #f))

  (define (vhint-set! idx val)
    (when (>= idx varc)
      (vector-set! vhint (- idx varc) val)))
  (define (vhint-ref idx)
    (if (>= idx varc)
        (vector-ref vhint (- idx varc))
        #f))

  (for ([root-reg (in-vector rootvec)])
    (vhint-set! root-reg #t))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [hint (in-vector vhint (- (vector-length vhint) 1) -1 -1)]
        [n (in-range (- (vector-length vhint) 1) -1 -1)]
        #:when hint)
    (define hint*
      (case (object-name (car instr))
        [(ival-if)
         (match-define (list _ cond tru fls) instr)
         (define cond-reg (vector-ref vregs cond))
         (cond
           [(and (ival-lo cond-reg) (ival-hi cond-reg))
            (vhint-set! cond (or #f (vhint-ref cond)))
            (vhint-set! tru #t)
            (vhint-set! fls (or #f (vhint-ref fls)))
            2]
           [(not (or (ival-lo cond-reg) (ival-hi cond-reg)))
            (vhint-set! cond (or #f (vhint-ref cond)))
            (vhint-set! tru (or #f (vhint-ref tru)))
            (vhint-set! fls #t)
            3]
           [else
            (vhint-set! cond #t)
            (vhint-set! tru #t)
            (vhint-set! fls #t)
            #t])]
        [(ival-fmax)
         (match-define (list _ arg1 arg2) instr)
         (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
         (cond
           [(and (ival-lo cmp) (ival-hi cmp))
            (vhint-set! arg2 (or #f (vhint-ref arg2)))
            (vhint-set! arg1 #t)
            1]
           [(not (or (ival-lo cmp) (ival-hi cmp)))
            (vhint-set! arg1 (or #f (vhint-ref arg1)))
            (vhint-set! arg2 #t)
            2]
           [else
            (vhint-set! arg1 #t)
            (vhint-set! arg2 #t)
            #t])]
        [(ival-fmin)
         (match-define (list _ arg1 arg2) instr)
         (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
         (cond
           [(and (ival-lo cmp) (ival-hi cmp))
            (vhint-set! arg1 (or #f (vhint-ref arg1)))
            (vhint-set! arg2 #t)
            2]
           [(not (or (ival-lo cmp) (ival-hi cmp)))
            (vhint-set! arg2 (or #f (vhint-ref arg2)))
            (vhint-set! arg1 #t)
            1]
           [else
            (vhint-set! arg1 #t)
            (vhint-set! arg2 #t)
            #t])]
        [else
         (define srcs (rest instr))
         (map (λ (x) (vhint-set! x #t)) srcs)
         #t]))
    (println "done")
    (vector-set! vhint n hint*))
  vhint)

(define (backward-pass machine)
  ; Since Step 2 writes into *sampling-iteration* if the max prec was reached - save the iter number for step 3
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define slackvec (rival-machine-output-distance machine))
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define vrepeats (rival-machine-repeats machine))
  (define vprecs (rival-machine-precisions machine))
  (define vstart-precs (rival-machine-incremental-precisions machine))
  (define current-iter (rival-machine-iteration machine))
  (define bumps (rival-machine-bumps machine))

  (define varc (vector-length args))
  (define vprecs-new (make-vector (vector-length ivec) 0)) ; new vprecs vector

  ; Step 1. Adding slack in case of a rounding boundary issue
  (for ([root-reg (in-vector rootvec)]
        [disc (in-vector discs)]
        [out-dr? (in-vector slackvec)]
        #:when (>= root-reg varc)
        #:when out-dr?)
    (vector-set! vprecs-new (- root-reg varc) (get-slack)))

  ; Step 1b. Checking if a operation should be computed again at all
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

  ; Step 2. Precision tuning
  (precision-tuning ivec vregs vprecs-new varc vstart-precs vuseful)

  ; Step 3. Repeating precisions check + Assigning if a operation should be computed again at all
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (define any-false? #f)
  (for ([instr (in-vector ivec)]
        [useful? (in-vector vuseful)]
        [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
        [prec-new (in-vector vprecs-new)]
        [result-old (in-vector vregs varc)]
        [n (in-naturals)])
    (define repeat
      (or (not useful?)
          (and (<= prec-new prec-old)
               (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc)))) (cdr instr)))))
    (set! any-false? (or any-false? (not repeat)))
    (vector-set! vrepeats n repeat))

  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new)

  ; Step 5. If precisions have not changed but the point didn't converge.
  ; A problem exists - add slack to every op
  ; Exit if precision has exceeded rival-max-precision and lower-bound-early-stopping is turned off
  (unless any-false?
    (set-rival-machine-bumps! machine (add1 bumps))
    (define slack (get-slack))
    (for ([prec (in-vector vprecs)]
          [n (in-range (vector-length vprecs))])
      (define prec* (min (*rival-max-precision*) (+ prec slack)))
      (when (and (not (*lower-bound-early-stopping*)) (equal? prec* (*rival-max-precision*)))
        (*sampling-iteration* (*rival-max-iterations*)))
      (vector-set! vprecs n prec*))
    (vector-fill! vrepeats #f)))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking, the upper precision bound is calculated as:
;   vprecs-max[i] = (+ max-prec vstart-precs[i]), where min-prec < (+ max-prec vstart-precs[i]) < max-prec
;   max-prec = (car (get-bounds parent))
(define (precision-tuning ivec vregs vprecs-max varc vstart-precs vuseful)
  (define vprecs-min (make-vector (vector-length ivec) 0))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)]
        [n (in-range (- (vector-length vregs) 1) -1 -1)]
        #:when useful?)
    (define op (car instr))
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers))
    (define output (vector-ref vregs n))

    (define max-prec (vector-ref vprecs-max (- n varc))) ; upper precision bound given from parent
    (define min-prec (vector-ref vprecs-min (- n varc))) ; lower precision bound given from parent

    ; Final precision assignment based on the upper bound
    (define final-precision
      (min (max (+ max-prec (vector-ref vstart-precs (- n varc))) (*rival-min-precision*))
           (*rival-max-precision*)))
    (vector-set! vprecs-max (- n varc) final-precision)

    ; Early stopping
    (match (*lower-bound-early-stopping*)
      [#t
       (when (>= min-prec (*rival-max-precision*))
         (*sampling-iteration* (*rival-max-iterations*)))]
      [#f
       (when (equal? final-precision (*rival-max-precision*))
         (*sampling-iteration* (*rival-max-iterations*)))])

    ; Precision propogation for each tail instruction
    (define ampl-bounds (get-bounds op output srcs)) ; amplification bounds for children instructions
    (for ([x (in-list tail-registers)]
          [bound (in-list ampl-bounds)]
          #:when (>= x varc)) ; when tail register is not a variable
      (match-define (cons up-bound lo-bound) bound)

      ; Upper precision bound propogation
      (vector-set! vprecs-max
                   (- x varc)
                   (max (vector-ref vprecs-max (- x varc)) (+ max-prec up-bound)))

      ; Lower precision bound propogation
      (vector-set! vprecs-min
                   (- x varc)
                   (max (vector-ref vprecs-min (- x varc)) (+ min-prec (max 0 lo-bound)))))))
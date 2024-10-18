#lang racket/base

(require "tricks.rkt"
         "../ops/all.rkt"
         "machine.rkt"
         racket/list)

(provide backward-pass)

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
  (define vstart-precs (rival-machine-initial-precisions machine))
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
  (precision-tuning ivec vregs vprecs-new varc vstart-precs)

  ; Step 3. Repeating precisions check + Checking if a operation should be computed again at all
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
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
    (vector-set! vrepeats n repeat))

  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-max[i] = min( *rival-max-precision*
;                        max( *base-tuning-precision* (+ parent-prec-upper-bound vstart-precs[i])),
;   parent-prec-upper-bound = (car (get-bounds(parent)))
(define (precision-tuning ivec vregs vprecs-max varc vstart-precs)
  (define vprecs-min (make-vector (vector-length vprecs-max) 0))
  ;(printf "----------- Backpass ~a ---------------\n" (*sampling-iteration*))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)] ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)]) ; reversed over indices of vregs
    (define op (car instr)) ; current operation
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n)) ; output of the current instr

    (define parent-prec-upper-bound
      (vector-ref vprecs-max (- n varc))) ; intro for the current instruction
    (define parent-prec-lower-bound (vector-ref vprecs-min (- n varc)))

    (when (>= parent-prec-lower-bound (*rival-max-precision*)) ; Early stopping
      (*sampling-iteration* (*rival-max-iterations*)))

    ; Final precision assignment
    (define instr-precision-upper-bound
      (max (+ parent-prec-upper-bound (vector-ref vstart-precs (- n varc)))
           (*base-tuning-precision*)))
    (vector-set! vprecs-max (- n varc) (min instr-precision-upper-bound (*rival-max-precision*)))

    ; Intro and ampl propogation for each tail instruction
    (define ampl-bounds (get-bounds op output srcs)) ; ampl bounds for children instructions

    ; ---------------- Debugging
    #;(printf "~a) ~a\n\toutput = ~a\n\tsrcs=~a\n\tampl=~a\n\tnew-prec=~a\n\tlower-bound=~a\n"
              n
              instr
              output
              srcs
              ampl-bounds
              instr-precision-upper-bound
              parent-prec-lower-bound)
    ; ----------------

    (for ([x (in-list tail-registers)]
          [bound (in-list ampl-bounds)]
          #:when (>= x varc)) ; when tail register is not a variable
      ; check whether this op already has a precision that is higher
      ; Upper bound propogation
      (when (> (+ parent-prec-upper-bound (first bound)) (vector-ref vprecs-max (- x varc)))
        (vector-set! vprecs-max (- x varc) (+ parent-prec-upper-bound (first bound))))

      ; This statement probably can be neglected and moved into above?
      ; Lower bound propogation
      (when (> (+ parent-prec-lower-bound (second bound)) (vector-ref vprecs-min (- x varc)))
        (vector-set! vprecs-min (- x varc) (+ parent-prec-lower-bound (second bound)))))))

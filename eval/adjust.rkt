#lang racket/base

(require "tricks.rkt"
         "../ops/all.rkt"
         "machine.rkt"
         racket/list
         racket/match)

(provide backward-pass
         make-hint)

; Hint is a vector with len(ivec) elements which
;   guides Rival on which instructions should not be executed
;   for points from a particular hyperrect of input parameters.
; make-hint is called as a last step of rival-analyze and returns hint as a result
; Values of a hint:
;   #f - instruction should not be executed
;   #t - instruction should be executed
;   integer n - instead of executing, refer to vregs with (list-ref instr n) index
;               (the result is known and stored in another register)
;   ival - instead of executing, just copy ival as a result of the instruction
(define (make-hint machine old-hint)
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define vregs (rival-machine-registers machine))

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
        [(? box? _) o-hint] ; box means that the result is known at some precision
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

(define (backward-pass machine vhint)
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
  (precision-tuning ivec vregs vprecs-new varc vstart-precs vuseful vhint)

  ; Step 3. Repeating precisions check + Assigning if a operation should be computed again at all
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (define (repeats)
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
    any-false?)

  ; Step 4. If precisions have not changed but the point didn't converge.
  ; Assign precisions again now on with logspan
  ; and do repeats optimization again on new precisions
  (unless (repeats)
    (set-rival-machine-bumps! machine (add1 bumps))
    (*bumps-activated* #t)
    (vector-fill! vprecs-new 0)
    (precision-tuning ivec vregs vprecs-new varc vstart-precs vuseful vhint)
    (repeats))

  ; Step 5. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking, the upper precision bound is calculated as:
;   vprecs-max[i] = (+ max-prec vstart-precs[i]), where min-prec < (+ max-prec vstart-precs[i]) < max-prec
;   max-prec = (car (get-bounds parent))
(define (precision-tuning ivec vregs vprecs-max varc vstart-precs vuseful vhint)
  (define vprecs-min (make-vector (vector-length ivec) 0))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)]
        [n (in-range (- (vector-length vregs) 1) -1 -1)]
        [hint (in-vector vhint (- (vector-length vhint) 1) -1 -1)]
        #:when (and hint useful?))
    (define op (car instr))
    (define tail-registers (cdr instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers))
    (define output (vector-ref vregs n))

    (define max-prec (vector-ref vprecs-max (- n varc))) ; upper precision bound given from parent
    (define min-prec (vector-ref vprecs-min (- n varc))) ; lower precision bound given from parent

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
                   (max (vector-ref vprecs-min (- x varc)) (+ min-prec (max 0 lo-bound)))))

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
         (*sampling-iteration* (*rival-max-iterations*)))])))

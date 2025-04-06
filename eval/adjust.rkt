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
            (define srcs
              (drop-self-pointer (rest instr)
                                  (+ n
                                     varc))) ; then, children instructions should be executed as well
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
  (define vinitial-repeats (rival-machine-initial-repeats machine))
  (define vprecs (rival-machine-precisions machine))
  (define vstart-precs (rival-machine-initial-precisions machine))
  (define vbest-precs (rival-machine-best-known-precisions machine))
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
  (vector-fill! vrepeats #t) ; #t - means it WON'T be REEXECUTED
  (for ([root (in-vector rootvec)]
        #:when (>= root varc))
    (vector-set! vrepeats (- root varc) #f)) ; #f - means it WILL be REEXECUTED
  (for ([reg (in-vector vregs (- (vector-length vregs) 1) (- varc 1) -1)]
        [instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [i (in-range (- (vector-length ivec) 1) -1 -1)]
        [repeat? (in-vector vrepeats (- (vector-length vrepeats) 1) -1 -1)]
        #:unless repeat?)
    (cond
      [(and (ival-lo-fixed? reg) (ival-hi-fixed? reg)) (vector-set! vrepeats i #t)]
      [else
       (for ([arg (in-list (drop-self-pointer (cdr instr) (+ i varc)))]
             #:when (>= arg varc))
         (vector-set! vrepeats (- arg varc) #f))]))

  ; Step 2. Precision tuning
  (precision-tuning ivec vregs vprecs-new varc vstart-precs vrepeats vhint)

  ; Step 3. Repeating precisions check + Assigning if a operation should be computed again at all
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (define (repeats)
    (define any-reevaluation? #f)
    (for ([instr (in-vector ivec)]
          [result-is-exact-already (in-vector vrepeats)]
          [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
          [prec-new (in-vector vprecs-new)]
          [best-known-precision (in-vector vbest-precs)]
          [constant? (in-vector vinitial-repeats)]
          [n (in-naturals)]
          #:unless result-is-exact-already)
      ; check whether precision has increased
      (define tail-registers (drop-self-pointer (cdr instr) (+ n varc)))
      (define precision-has-not-increased
        (and (<= prec-new (if constant? best-known-precision prec-old))
             (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc)))) tail-registers)))
      ; update constant precision
      (define precision-has-increased (not precision-has-not-increased))
      (when (and constant? precision-has-increased)
        (vector-set! vbest-precs n prec-new))
      (set! any-reevaluation? (or any-reevaluation? precision-has-increased))

      (vector-set! vrepeats n precision-has-not-increased))
    any-reevaluation?)
  (define any-reevaluation? (repeats))

  ; Step 4. If precisions have not changed but the point didn't converge.
  ; Assign precisions again now on with logspan
  ; and do repeats optimization again on new precisions
  (unless any-reevaluation?
    (set-rival-machine-bumps! machine (add1 bumps))
    (*bumps-activated* #t)
    ; clean progress of the current tuning pass and start over
    (vector-fill! vprecs-new 0)
    (vector-fill! vrepeats #f)
    (precision-tuning ivec vregs vprecs-new varc vstart-precs vrepeats vhint)
    (repeats)) ; do repeats again

  ; Step 5. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new))

; Usually, add-bang instructions have a pointer to itself that is needed to be dropped
(define (drop-self-pointer tail-regs n)
  (if (empty? tail-regs)
      tail-regs
      (if (equal? (car tail-regs) n)
          (cdr tail-regs)
          tail-regs)))

; This function goes through ivec and vregs and calculates (+ ampls base-precisions) for each operator in ivec
; Roughly speaking, the upper precision bound is calculated as:
;   vprecs-max[i] = (+ max-prec vstart-precs[i]), where min-prec < (+ max-prec vstart-precs[i]) < max-prec
;   max-prec = (car (get-bounds parent))
(define (precision-tuning ivec vregs vprecs-max varc vstart-precs vrepeats vhint)
  (define vprecs-min (make-vector (vector-length ivec) 0))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [repeat? (in-vector vrepeats (- (vector-length vrepeats) 1) -1 -1)]
        [n (in-range (- (vector-length vregs) 1) -1 -1)]
        [hint (in-vector vhint (- (vector-length vhint) 1) -1 -1)]
        [output (in-vector vregs (- (vector-length vregs) 1) -1 -1)]
        #:when (and hint (not repeat?)))
    (define op (car instr))
    (define tail-registers (drop-self-pointer (cdr instr) n))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers))

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

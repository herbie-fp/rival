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

(module+ test
  (require rackunit
           "compile.rkt"
           "../utils.rkt"
           math/bigfloat)
  (define number-of-random-hyperrects 100)
  (define number-of-random-pts-per-rect 100)
  (bf-precision 53)

  ; Check whether outputs are the same for the hint and without hint executions
  (define (rival-check-hint machine hint pt)
    (define no-hint-result
      (with-handlers ([exn:rival:invalid? (λ (e) 'invalid)]
                      [exn:rival:unsamplable? (λ (e) 'unsamplable)])
        (rival-apply machine pt)))
    (define no-hint-instr-count (vector-length (rival-profile machine 'executions)))

    (define hint-result
      (with-handlers ([exn:rival:invalid? (λ (e) 'invalid)]
                      [exn:rival:unsamplable? (λ (e) 'unsamplable)])
        (rival-apply machine pt hint)))
    (define hint-instr-count (vector-length (rival-profile machine 'executions)))
    (check-equal? hint-result no-hint-result)
    (values no-hint-instr-count hint-instr-count))

  ; Random sampling hyperrects given a general range as [rect-lo, rect-hi]
  (define (sample-hyperrect-within-bounds rect-lo rect-hi varc)
    (for/vector ([_ (in-range varc)])
      (define xlo-range-length (bf- rect-hi rect-lo))
      (define xlo (bf+ (bf* (bfrandom) xlo-range-length) rect-lo))
      (define xhi-range-length (bf- rect-hi xlo))
      (define xhi (bf+ (bf* (bfrandom) xhi-range-length) xlo))
      (check-true (and (bf> rect-hi xhi) (bf> xlo rect-lo) (bf> xhi xlo))
                  "Hyperrect is out of bounds")
      (ival xlo xhi)))

  ; Sample points with respect to the input hyperrect
  (define (sample-pts hyperrect)
    (for/vector ([rect (in-vector hyperrect)])
      (define range-length (bf- (ival-hi rect) (ival-lo rect)))
      (define pt (bf+ (bf* (bfrandom) range-length) (ival-lo rect)))
      (check-true (and (bf> pt (ival-lo rect)) (bf< pt (ival-hi rect)))
                  "Sampled point is out of hyperrect range")
      pt))

  ; Testing hint on an expression for 'number-of-random-hyperrects' hyperrects by
  ;     'number-of-random-pts-per-rect' points each
  (define (hints-random-checks machine rect-lo rect-hi varc)
    (define evaluated-instructions 0)
    (define number-of-instructions-total
      (* number-of-random-hyperrects (vector-length (rival-machine-instructions machine))))

    (define hint-cnt 0)
    (define no-hint-cnt 0)
    (for ([n (in-range number-of-random-hyperrects)])
      (define hyperrect (sample-hyperrect-within-bounds rect-lo rect-hi varc))
      (match-define (list res hint _) (rival-analyze machine hyperrect))
      (set! evaluated-instructions (+ evaluated-instructions (vector-count false? hint)))

      (for ([_ (in-range number-of-random-pts-per-rect)])
        (define pt (sample-pts hyperrect))
        (define-values (no-hint-cnt* hint-cnt*) (rival-check-hint machine hint pt))
        (set! hint-cnt (+ hint-cnt hint-cnt*))
        (set! no-hint-cnt (+ no-hint-cnt no-hint-cnt*))))

    (define skipped-percentage (* (/ hint-cnt no-hint-cnt) 100))
    skipped-percentage)

  (define discs (list boolean-discretization flonum-discretization))
  (define vars '(x y))
  (define varc (length vars))

  (define expr1
    (list '(assert (> (+ (log x) (log y)) (- (log x) (log y))))
          '(+ (if (> (/ (log x) (log y)) (* (log x) (log y)))
                  (fmax (* (log x) (log y)) (+ (log x) (log y)))
                  (fmin (* (log x) (log y)) (+ (log x) (log y))))
              (if (> (+ (log x) (log y)) (* (log x) (log y)))
                  (fmax (/ (log x) (log y)) (- (log x) (log y)))
                  (fmin (/ (log x) (log y)) (- (log x) (log y)))))))
  (define machine1 (rival-compile expr1 vars discs))
  (define skipped-instr1 (hints-random-checks machine1 (bf -100) (bf 100) varc))
  (printf "Percentage of skipped instructions by hint in expr1 = ~a\n" (round skipped-instr1))

  (define expr2
    (list '(TRUE)
          '(fmax (fmin (fmax (* x y) (+ x y)) (+ (fmax x (* 2 y)) (fmin y (* x 2))))
                 (fmax (fmin (* x y) (+ x y)) (+ (fmin x (* 2 y)) (fmax y (* x 2)))))))
  (define machine2 (rival-compile expr2 vars discs))
  (define skipped-instr2 (hints-random-checks machine2 (bf -100) (bf 100) varc))
  (printf "Percentage of skipped instructions by hint in expr2 = ~a\n" (round skipped-instr2))

  (define expr3
    (list '(TRUE)
          '(if (> (exp x) (+ 10 (log y)))
               (if (> (fmax (* x y) (+ x y)) 4)
                   (cos (fmax x y))
                   (cos (fmin x y)))
               (if (< (pow 2 x) (- (exp x) 10))
                   (* PI x)
                   (fmax x (- (cos y) (+ 10 (log y))))))))
  (define machine3 (rival-compile expr3 vars discs))
  (define skipped-instr3 (hints-random-checks machine3 (bf -100) (bf 100) varc))
  (printf "Percentage of skipped instructions by hint in expr3 = ~a\n" (round skipped-instr3))

  ; Test checks hint on assert where an error can be observed
  (define expr4 (list '(assert (> (+ (log x) (log y)) (- (log x) (log y)))) '(+ (cos x) (cos y))))
  (define machine4 (rival-compile expr4 vars discs))
  (define skipped-instr4 (hints-random-checks machine4 (bf -100) (bf 100) varc))
  (printf "Percentage of skipped instructions by hint in expr1 = ~a\n" (round skipped-instr4)))

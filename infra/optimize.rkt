#lang racket

(require json
         math/bigfloat)
(require "../main.rkt"
         "../eval/machine.rkt"
         "../eval/optimal.rkt"
         "../utils.rkt")

(define (read-from-string s)
  (read (open-input-string s)))

(define (analyze-program rec bench-id min-speedup output-port)
  (define exprs (map read-from-string (hash-ref rec 'exprs)))
  (define vars (map read-from-string (hash-ref rec 'vars)))
  (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))
  (define discs (cons boolean-discretization (map (const flonum-discretization) (cdr exprs))))

  (define machine
    (parameterize ([*rival-max-precision* 32256])
      (rival-compile exprs vars discs)))

  (define results
    (filter
     identity
     (for/list ([pt* (in-list (hash-ref rec 'points))]
                [pt-id (in-naturals)])
       (match-define (list pt _sollya-exs _sollya-status _sollya-apply-time) pt*)
       (define pt-vec
         (parameterize ([bf-precision 53])
           (list->vector (map bf pt))))
       (define result (rival-machine-find-optimal-precisions machine pt-vec))
       (match result
         [(list optimal-precs optimal-time cur-precs cur-time) (list pt-id pt optimal-time cur-time)]
         [#f
          (eprintf "; Benchmark ~a point ~a, failure to optimize\n" bench-id pt-id)
          #f]))))

  (define (bad-pt? rec)
    (match-define (list pt-id pt opt-time cur-time) rec)
    (and (> cur-time (* min-speedup opt-time))
         (> (- cur-time opt-time) .001))) ; At least 1 us of speedup!

  (define dt (* 1000 (- (apply + (map fourth results)) (apply + (map third results)))))
  (define valid-results (sort (filter bad-pt? results) > #:key fourth)) ; Sort by cur-time
  (eprintf "; Benchmark ~a, total ~aµs available, ~a bad points\n"
           bench-id
           (~r dt #:precision '(= 1))
           (length valid-results))
  (unless (empty? valid-results)
    (fprintf output-port "; Benchmark ~a, total ~aµs available\n" bench-id (~r dt #:precision '(= 1)))
    (fprintf output-port
             "(define (b~a ~a)\n  ~a)\n"
             bench-id
             (string-join (map ~s vars) " ")
             (string-join (map ~s exprs) " "))

    (for ([result (in-list valid-results)]
          [n (in-range 10)]) ; At most 10
      (match-define (list pt-id pt opt-time cur-time) result)
      (fprintf output-port "(optimize b~a ~a)\n" bench-id (string-join (map ~a pt) " ")))
    (fprintf output-port "\n")))

(module+ main
  (require racket/cmdline)
  (define min-speedup (make-parameter 1.2))
  (command-line
   #:once-each [("--min") n "Minimum speedup to report" (min-speedup (string->number n))]
   #:args ([points-file "infra/points.json"] [output-file "optimaize.rival"])
   (printf "Analyzing points bad precision assignment (min speedup: ~a)...\n\n" (min-speedup))
   (call-with-output-file
    output-file
    #:exists 'replace
    (λ (out-port)
      (call-with-input-file points-file
                            (λ (input)
                              (for ([rec (in-port read-json input)]
                                    [bench-id (in-naturals)])
                                (analyze-program rec bench-id (min-speedup) out-port))))))))

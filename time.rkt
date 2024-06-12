#lang racket

(require racket/math math/base math/flonum math/bigfloat racket/random)
(require json)
(require "main.rkt" "test.rkt")

(define sample-vals (make-parameter 5000))

(define (time-operation ival-fn itypes otype)
  (define n
    (if (set-member? slow-tests ival-fn)
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (define times (make-vector n))
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (sample-interval itype)))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (define dt (- (current-inexact-milliseconds) start))
    (vector-set! times i dt))
  (vector->list times))

(define (time-operations)
  (for/list ([fn (in-list function-table)])
    (match-define (list ival-fn bf-fn itypes otype) fn)
    (define t (time-operation ival-fn itypes otype))
    (define avg (/ (apply + t) (length t)))
    (define stdev (sqrt (/ (apply + (for/list ([v t]) (expt (- v avg) 2))) (- (length t) 1.5))))
    (define serr (/ stdev (sqrt (length t))))
    (list ival-fn avg serr)))

(define (read-from-string s)
  (read (open-input-string s)))

(define (time-expr rec)
  (define exprs (map read-from-string (hash-ref rec 'exprs)))
  (define vars (map read-from-string (hash-ref rec 'vars)))
  (unless (andmap symbol? vars)
    (raise 'time "Invalid variable list ~a" vars))
  (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))
  (define discs (cons boolean-discretization (map (const flonum-discretization) (cdr exprs))))
  (define start-compile (current-inexact-milliseconds))
  (define machine (rival-compile exprs vars discs))
  (define compile-time (- (current-inexact-milliseconds) start-compile))

  (define times
    (for/list ([pt (in-list (hash-ref rec 'points))])
      (define start-apply (current-inexact-milliseconds))
      (define status
        (with-handlers ([exn:rival:invalid? (const 'invalid)]
                        [exn:rival:unsamplable? (const 'unsamplable)])
          (rival-apply machine (list->vector (map bf pt)))
          'valid))
      (define apply-time (- (current-inexact-milliseconds) start-apply))
      (cons status apply-time)))

  (cons (cons 'compile compile-time) times))

(define (time-exprs data)
  (define times
    (for/hash ([group (in-list (group-by car data))])
      (values (caar group) (map cdr group))))

  (list (car (hash-ref times 'compile))
        (length (hash-ref times 'valid '()))
        (/ (apply + (hash-ref times 'valid '())) 1000)
        (length (hash-ref times 'invalid '()))
        (/ (apply + (hash-ref times 'invalid '())) 1000)
        (length (hash-ref times 'unsamplable '()))
        (/ (apply + (hash-ref times 'unsamplable '())) 1000)))

(define (run html? test-id p)
  (when html? (printf "<!doctype html>"))
  
  (unless test-id
    (when html?
      (printf "<h1>Operation timings</h1>")
      (printf "<table>")
      (printf "<thead><tr><th>Operation<th colspan=2>Time ([min, max])")
      (printf "<tbody>"))
    (for ([rec (in-list (time-operations))])
      (match-define (list ival-fn avg se) rec)
      (define min-s (~r (* (- avg se se) 1000) #:precision '(= 3)))
      (define max-s (~r (* (+ avg se se) 1000) #:precision '(= 3)))
      (cond
        [html?
         (printf "<tr><td><code>~a</code></td>" (object-name ival-fn))
         (printf "<td>~aµs<td>~aµs" min-s max-s)]
        [else
         (printf "~a [~a, ~a]µs\n"
              (~a (object-name ival-fn) #:align 'left #:min-width 20)
              (~a min-s #:min-width 8)
              (~a max-s #:min-width 8))]))
    (when html?
      (printf "</table>")))

  (when p
    (cond
      [html?
       (printf "<h1>Expression Timing</h1>")
       (printf "<table>")
       (printf "<thead><tr><th>#<th>Time (s)<th>Compile (s)<th colspan=2>Valid (#, s)<th colspan=2>Invalid (#, s)<th colspan=2>Unsamplable (#, s)</thead>")]
      [else
       (newline)])
    (define total-c 0.0)
    (define total-v 0.0)
    (define count-v 0.0)
    (define total-i 0.0)
    (define count-i 0.0)
    (define total-u 0.0)
    (define count-u 0.0)

    (for ([rec (in-port read-json p)] [i (in-naturals)] #:unless (and test-id (not (= i test-id))))
      (when test-id
        (pretty-print (map read-from-string (hash-ref rec 'exprs))))
      (match-define (list c-time v-num v-time i-num i-time u-num u-time)
        (time-exprs (time-expr rec)))
      (set! total-c (+ total-c c-time))
      (set! total-v (+ total-v v-time))
      (set! count-v (+ count-v v-num))
      (set! total-i (+ total-i i-time))
      (set! count-i (+ count-i i-num))
      (set! total-u (+ total-u u-time))
      (set! count-u (+ count-u u-num))
      
      (cond
        [html?
         (printf "<tr><td>~a<td>~as" i (+ c-time v-time i-time u-time))
         (printf "<td>~a" (~r c-time #:precision '(= 3)))
         (if (> v-num 0)
             (printf "<td>~a<td>~as" v-num (~r v-time #:precision '(= 3)))
             (printf "<td><td>"))
         (if (> i-num 0)
             (printf "<td>~a<td>~as" i-num (~r i-time #:precision '(= 3)))
             (printf "<td><td>"))
         (if (> u-num 0)
             (printf "<td>~a<td>~as" u-num (~r u-time #:precision '(= 3)))
             (printf "<td><td>"))]
        [else
         (printf "~a ~ams v(~a: ~ams) i(~a: ~ams) u(~a: ~ams)\n"
                 (~a i #:align 'left #:min-width 3)
                 (~r c-time #:precision '(= 3) #:min-width 8)
                 (~a v-num #:min-width 5)
                 (~r v-time #:precision '(= 3) #:min-width 8)
                 (~a i-num #:min-width 5)
                 (~r i-time #:precision '(= 3) #:min-width 8)
                 (~a u-num #:min-width 5)
                 (~r u-time #:precision '(= 3) #:min-width 8))]))

    (cond
      [html?
       (printf "<tbody><tr><td>Total<td>~a<td>~a"
               (~r (+ total-c total-v total-i total-u) #:precision '(= 3))
               (~r total-c #:precision '(= 3)))
       (printf "<td><td>~a<td><td>~a<td><td>~a"
               (~r total-v #:precision '(= 3))
               (~r total-i #:precision '(= 3))
               (~r total-u #:precision '(= 3)))
       (printf "</table>")]
      [else
       (printf "\nTotal Time: ~as\n" (~r (+ total-c total-v total-i total-u) #:precision '(= 3)))])))


(module+ main
  (require racket/cmdline)
  (define html? #f)
  (define n #f)
  (command-line
   #:once-each
   [("--html") "Produce HTML output"
               (set! html? #t)]
   [("--id") ns "Run a single test"
             (set! n (string->number ns))]
   #:args ([points "infra/points.json"])
   (run html? n (open-input-file points))))

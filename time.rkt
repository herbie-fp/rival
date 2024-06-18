#lang racket

(require racket/math math/base math/flonum math/bigfloat racket/random)
(require json)
(require "main.rkt" "test.rkt" "infra/run-mathematica.rkt")
(provide time-expr)

(define sample-vals (make-parameter 5000))

(define (time-operation ival-fn bf-fn itypes otype)
  (define n
    (if (set-member? slow-tests ival-fn)
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (define ivtime 0.0)
  (define bftime 0.0)
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (sample-interval itype)))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (define dt1 (- (current-inexact-milliseconds) start))
    (set! ivtime (+ ivtime dt1))
    (define start2 (current-inexact-milliseconds))
    (apply bf-fn (map ival-lo args))
    (apply bf-fn (map ival-hi args))
    (define dt2 (- (current-inexact-milliseconds) start2))
    (set! bftime (+ bftime dt2)))
  (list (* 1000 (/ ivtime n)) (* 1000 (/ bftime n))))

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
  (when html?
    (printf "<!doctype html><meta charset=utf-8 />")
    (define sortable-css "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.css")
    (define sortable-js "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.js")
    (printf "<link href='~a' rel='stylesheet' />" sortable-css)
    (printf "<script src='~a' async defer></script>" sortable-js)
    (printf "<style>tbody td:nth-child(1n+2) { text-align: right; }</style>"))

  (when (or (not test-id) (equal? test-id "ops"))
    (when html?
      (printf "<h1>Operation timings</h1>")
      (printf "<table class=sortable>")
      (printf "<thead><tr><th>Operation<th>Time, 256b<th>Slowdown")
      (printf "<th>Time, 4kb<th>Slowdown")
      (printf "<tbody>"))
    (for ([fn (in-list function-table)])
      (match-define (list ival-fn bf-fn itypes otype) fn)
      (match-define (list iv256 bf256) (time-operation ival-fn bf-fn itypes otype))
      (match-define (list iv4k bf4k)
        (if (set-member? slow-tests ival-fn)
            (list #f #f)
            (parameterize ([bf-precision 4096])
              (time-operation ival-fn bf-fn itypes otype))))
      (cond
        [html?
         (printf "<tr><td><code>~a</code></td>" (object-name ival-fn))
         (printf "<td data-sort=~a>~aµs" iv256 (~r iv256 #:precision '(= 3)))
         (printf "<td data-sort=~a>~a×" (/ iv256 bf256) (~r (/ iv256 bf256) #:precision '(= 2)))
         (printf "<td data-sort=~a>~aµs" (if iv4k iv4k "1000000") (if iv4k (~r iv4k #:precision '(= 3)) "∞"))
         (printf "<td data-sort=~a>~a×" (if (and iv4k bf4k) (/ iv4k bf4k) "1000000") (if (and iv4k bf4k) (~r (/ iv4k bf4k) #:precision '(= 2)) "∞"))]
        [else
         (printf "~a ~aµs ~a×\t~aµs ~a×\n"
              (~a (object-name ival-fn) #:align 'left #:min-width 20)
              (~r iv256 #:precision '(= 3) #:min-width 8)
              (~r (/ iv256 bf256) #:precision '(= 2) #:min-width 4)
              (~r iv4k #:precision '(= 3) #:min-width 8)
              (~r (/ iv4k bf4k) #:precision '(= 2) #:min-width 4))]))
    (when html?
      (printf "</table>")))

  (when p
    (cond
      [html?
       (printf "<h1>Expression Timing</h1>")
       (printf "<table class=sortable>")
       (printf "<thead><tr><th>#<th>Time (s)<th>Compile (s)<th>Valid<th>(s)<th>Invalid<th>(s)<th>Unsamplable<th>(s)</thead>")
       (printf "<tbody>")]
      [else
       (newline)])
    (define total-c 0.0)
    (define total-v 0.0)
    (define count-v 0.0)
    (define total-i 0.0)
    (define count-i 0.0)
    (define total-u 0.0)
    (define count-u 0.0)

    (for ([rec (in-port read-json p)] [i (in-naturals)]
                                      #:unless (and test-id (not (equal? (~a i) test-id))))
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
      (define t-time (+ c-time v-time i-time u-time))
      
      (cond
        [html?
         (printf "<tr><td title='~a'>~a<td data-sort=~a>~as" (second (hash-ref rec 'exprs)) i t-time (~r t-time #:precision '(= 3)))
         (printf "<td data-sort~a>~a" c-time (~r c-time #:precision '(= 3)))
         (if (> v-num 0)
             (printf "<td>~a<td data-sort=~a>~as" v-num v-time (~r v-time #:precision '(= 3)))
             (printf "<td><td>"))
         (if (> i-num 0)
             (printf "<td>~a<td data-sort=~a>~as" i-num i-time (~r i-time #:precision '(= 3)))
             (printf "<td><td>"))
         (if (> u-num 0)
             (printf "<td>~a<td data-sort=~a>~as" u-num u-time (~r u-time #:precision '(= 3)))
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
       (printf "<tfoot><tr><td>Total<td>~a<td>~a"
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
             (set! n ns)]
   #:args ([points "infra/points.json"])
   (run html? n (open-input-file points))))

#lang racket/base

(require racket/match racket/string racket/format)
(require (only-in math/private/bigfloat/mpfr bfcopy bigfloats-between bf-precision bigfloat->string bf))
(require "main.rkt" "machine.rkt")
(provide rival-repl)

(define (bf-discretization n)
  (discretization
   (lambda (x) (parameterize ([bf-precision n]) (bfcopy x)))
   (lambda (x y) (parameterize ([bf-precision n]) (abs (bigfloats-between x y))))))

(define (fix-up-fpcore expr)
  (match expr
    [`PI '(PI)]
    [`E '(E)]
    [`(,op ,args ...) (list* op (map fix-up-fpcore args))]
    [_ expr]))

(define (normalize-function-name name)
  (if (string-prefix? name "ival-")
      (substring name 5)
      name))

(define (executions-iterations execs)
  (define iter 0)
  (define last #f)
  (for/list ([exec (in-vector execs)])
    (match-define (execution name id precision time) exec)
    (when (and last (< id last))
      (set! iter (+ iter 1)))
    (set! last id)
    (cons iter exec)))

(define (write-table fn #:rows rows #:cols cols #:width [width 8])
  (for ([row (in-range rows)])
    (for ([col (in-range cols)])
      (display (~a (fn row col) #:width width #:align 'right)))
    (newline)))

(define-syntax-rule (list-find-match l pattern body ...)
  (let loop ([l l])
    (match l
      [(cons pattern rest)
       body ...]
      [(cons _ rest)
       (loop rest)]
      ['()
       ""])))

(define (rival-repl)
  (parameterize ([read-decimal-as-inexact #f])
    (define fns (make-hash))
    (define precision 53)
    (display "> ")
    (for ([i (in-port read (current-input-port))])
      (match i
        [`(set precision ,(? integer? n))
         (when (< n 4)
           (raise-user-error 'set "Precision must be an integer greater than 3"))
         (set! precision n)]
        [`(define (,(? symbol? name) ,(? symbol? args) ...)
            ,body)
         (hash-set! fns name
                    (rival-compile (list (fix-up-fpcore body)) args
                                   (list (bf-discretization precision))))]
        [`(explain ,(? symbol? name) ,(? real? vals) ...)
         (define machine (hash-ref fns name))
         (unless (= (vector-length (rival-machine-arguments machine)) (length vals))
           (define args (rival-machine-arguments machine))
           (raise-user-error name "Expects ~a arguments: ~a"
                             (length args) (string-join " " (map symbol->string args))))
         (define args
           (parameterize ([bf-precision precision])
             (list->vector (map bf vals))))

         ;; Make sure the cache is warm
         (rival-apply machine args)
         ;; Make sure the profile is clear
         (rival-profile machine 'executions)

         ;; Time the actual execution
         (define start (current-inexact-milliseconds))
         (rival-apply machine args)
         (define end (current-inexact-milliseconds))

         (define execs (rival-profile machine 'executions))
         (define num-instructions (rival-profile machine 'instructions))
         (define num-iterations (+ 1 (rival-profile machine 'iterations)))
         (define num-args (vector-length (rival-machine-arguments machine)))
         (printf "Executed ~a instructions for ~a iterations:\n\n" num-instructions num-iterations)

         (define execs* (executions-iterations execs))
         (write-table
          #:rows (+ 5 num-instructions) ; 1 for the "adjust" row
          #:cols (+ 1 (* 2 num-iterations))
          #:width 6
          (lambda (row col)
            (match* (row col)
              [(0 0) ""]
              [(0 col) #:when (= (modulo col 2) 1) "Bits"]
              [(0 col) #:when (= (modulo col 2) 0) "Time"]
              [(1 _) "------"]
              [(2 0) 'adjust]
              [(2 col) #:when (and (= (modulo col 2) 0) (> col 2))
               (define iter (- (/ col 2) 1))
               (list-find-match execs* (cons (== iter) (execution 'adjust _ _ time))
                 (~r (* time 1000) #:precision '(= 1)))]
              [(2 col) ""]
              [((== (+ 3 num-instructions)) _) "------"]
              [((== (+ 4 num-instructions)) 0) "Total"]
              [((== (+ 4 num-instructions)) col) #:when (= (modulo col 2) 1) ""]
              [((== (+ 4 num-instructions)) col) #:when (= (modulo col 2) 0)
               (define iter (/ (- col 2) 2))
               (define time
                 (apply + (for/list ([exec (in-list execs*)] #:when (= (car exec) iter))
                            (execution-time (cdr exec)))))
               (~r (* time 1000) #:precision '(= 1))]
              [(row 0)
               (define id (+ (- row 3) num-args))
               (list-find-match execs* (cons _ (execution name (== id) _ _))
                 (normalize-function-name (~a name)))]
              [(row col) #:when (= (modulo col 2) 1) ; precision
               (define id (+ (- row 3) num-args))
               (define iter (/ (- col 1) 2))
               (list-find-match execs* (cons (== iter) (execution _ (== id) prec _))
                 prec)]
              [(row col) #:when (= (modulo col 2) 0) ; time
               (define id (+ (- row 3) num-args))
               (define iter (/ (- col 2) 2))
               (list-find-match execs* (cons (== iter) (execution _ (== id) _ time))
                 (~r (* time 1000) #:precision '(= 1)))])))

         (printf "\n\nTotal: ~ams\n" (~r (- end start) #:precision '(= 3)))]
        [`(eval ,body)
         (define machine (rival-compile (list (fix-up-fpcore body)) '()
                                        (list (bf-discretization precision))))
         (define out
           (parameterize ([bf-precision precision])
             (vector-ref (rival-apply machine (vector)) 0)))
         (display (bigfloat->string out))
         (newline)]
        [`(eval ,(? symbol? name) ,(? real? vals) ...)
         (define machine (hash-ref fns name))
         (unless (= (vector-length (rival-machine-arguments machine)) (length vals))
           (define args (rival-machine-arguments machine))
           (raise-user-error name "Expects ~a arguments: ~a"
                             (length args) (string-join " " (map symbol->string args))))
         (define out
           (parameterize ([bf-precision precision])
             (vector-ref (rival-apply machine (list->vector (map bf vals))) 0)))
         (display (bigfloat->string out))
         (newline)])
      (display "> "))
    (display "exit")
    (newline)))

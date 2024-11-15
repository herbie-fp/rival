#lang racket

(require (only-in math/private/bigfloat/mpfr
                  bfcopy
                  bigfloats-between
                  bf-precision
                  bigfloat->string
                  bf))
(require "eval/main.rkt"
         "eval/machine.rkt"
         "utils.rkt")
(provide rival-repl)

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
      [(cons _ rest) (loop rest)]
      ['() ""])))

(struct repl ([precision #:mutable] context))

(define (make-repl [precision 53])
  (repl precision (make-hash)))

(define (repl-discretizations repl)
  (list (bf-discretization (repl-precision repl))))

(define (repl-get-machine repl name)
  (if (symbol? name)
      (hash-ref (repl-context repl) name)
      (rival-compile (list (fix-up-fpcore name)) '() (repl-discretizations repl))))

(define (repl-apply repl machine vals)
  (with-handlers ([exn:rival:invalid? (const "Domain error")]
                  [exn:rival:unsamplable? (const "Could not evaluate")])
    (parameterize ([bf-precision (repl-precision repl)])
      (vector-ref (rival-apply machine (list->vector (map bf vals))) 0))))

(define (repl-save-machine! repl name args body)
  (hash-set!
   (repl-context repl)
   name
   (rival-compile (list (fix-up-fpcore body)) args (list (bf-discretization (repl-precision repl))))))

(define (check-args! name machine vals)
  (unless (= (vector-length (rival-machine-arguments machine)) (length vals))
    (define args (rival-machine-arguments machine))
    (raise-user-error name
                      "Expects ~a arguments: ~a"
                      (length args)
                      (string-join " " (map symbol->string args)))))

(define (write-explain machine)
  (define execs (rival-profile machine 'executions))
  (define num-instructions (rival-profile machine 'instructions))
  (define num-iterations (+ 1 (rival-profile machine 'iterations)))
  (define num-args (vector-length (rival-machine-arguments machine)))
  (printf "Executed ~a instructions for ~a iterations:\n\n" num-instructions num-iterations)

  (define execs* (executions-iterations execs))
  (write-table #:rows (+ 5 num-instructions) ; 1 for the "adjust" row
               #:cols (+ 1 (* 2 num-iterations))
               #:width 6
               (lambda (row col)
                 (match* (row col)
                   [(0 0) ""]
                   [(0 col)
                    #:when (= (modulo col 2) 1)
                    "Bits"]
                   [(0 col)
                    #:when (= (modulo col 2) 0)
                    "Time"]
                   [(1 _) "------"]
                   [(2 0) 'adjust]
                   [(2 col)
                    #:when (and (= (modulo col 2) 0) (> col 2))
                    (define iter (- (/ col 2) 1))
                    (list-find-match execs*
                                     (cons (== iter) (execution 'adjust _ _ time))
                                     (~r (* time 1000) #:precision '(= 1)))]
                   [(2 col) ""]
                   [((== (+ 3 num-instructions)) _) "------"]
                   [((== (+ 4 num-instructions)) 0) "Total"]
                   [((== (+ 4 num-instructions)) col)
                    #:when (= (modulo col 2) 1)
                    ""]
                   [((== (+ 4 num-instructions)) col)
                    #:when (= (modulo col 2) 0)
                    (define iter (/ (- col 2) 2))
                    (define time
                      (apply +
                             (for/list ([exec (in-list execs*)]
                                        #:when (= (car exec) iter))
                               (execution-time (cdr exec)))))
                    (~r (* time 1000) #:precision '(= 1))]
                   [(row 0)
                    (define id (+ (- row 3) num-args))
                    (list-find-match execs*
                                     (cons _ (execution name (== id) _ _))
                                     (normalize-function-name (~a name)))]
                   [(row col)
                    #:when (= (modulo col 2) 1) ; precision
                    (define id (+ (- row 3) num-args))
                    (define iter (/ (- col 1) 2))
                    (list-find-match execs* (cons (== iter) (execution _ (== id) prec _)) prec)]
                   [(row col)
                    #:when (= (modulo col 2) 0) ; time
                    (define id (+ (- row 3) num-args))
                    (define iter (/ (- col 2) 2))
                    (list-find-match execs*
                                     (cons (== iter) (execution _ (== id) _ time))
                                     (~r (* time 1000) #:precision '(= 1)))]))))

(define (rival-repl p)
  (let/ec
   k
   (parameterize ([read-decimal-as-inexact #f]
                  [*rival-name-constants* #t])
     (define repl (make-repl))
     (when (terminal-port? p)
       (display "> "))
     (for ([cmd (in-port read p)])
       (match cmd
         [`(set precision ,(? integer? n))
          (when (< n 4)
            (raise-user-error 'set "Precision must be an integer greater than 3"))
          (set-repl-precision! repl n)]
         [`(define (,(? symbol? name) ,(? symbol? args) ...)
             ,body)
          (repl-save-machine! repl name args body)]
         [`(eval ,name ,(? real? vals) ...)
          (define machine (repl-get-machine repl name))
          (check-args! name machine vals)
          (define out (repl-apply repl machine vals))
          (displayln (if (string? out)
                         out
                         (bigfloat->string out)))]
         [`(explain ,name ,(? real? vals) ...)
          (define machine (repl-get-machine repl name))
          (check-args! name machine vals)

          ;; Make sure the cache is warm
          (repl-apply repl machine vals)
          ;; Make sure the profile is clear
          (rival-profile machine 'executions)

          ;; Time the actual execution
          (define start (current-inexact-milliseconds))
          (repl-apply repl machine vals)
          (define end (current-inexact-milliseconds))

          (write-explain machine)

          (printf "\nTotal: ~aµs\n" (~r (* (- end start) 1000) #:precision '(= 1)))]
         [(or '(help) 'help)
          (displayln "This is the Rival REPL, a demo of the Rival real evaluator.")
          (newline)
          (displayln "Commands:")
          (displayln "  (set precision <n>)                  Set working precision to n")
          (displayln "  (define (<name> <args> ...) <body>)  Define a named function")
          (displayln "  (eval <name> <vals> ...)             Evaluate a named function")
          (displayln
           "  (explain <name> <vals> ...)          Show profile for evaluating a named function")
          (newline)
          (displayln "A closed expression can always be used in place of a named function.")]
         [(or '(exit) 'exit) (k)]
         [_ (printf "Unknown command ~a; use help for command list\n" cmd)])
       (when (terminal-port? p)
         (display "> "))))
   (when (terminal-port? p)
     (displayln "exit"))))

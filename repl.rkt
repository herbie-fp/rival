#lang racket

(require (only-in math/private/bigfloat/mpfr
                  bfcopy
                  bigfloats-between
                  bf-precision
                  bigfloat->string
                  bigfloat?
                  bf))
(require "eval/main.rkt"
         "eval/machine.rkt"
         "utils.rkt")
(provide rival-repl)

(define (create-discs args bodies repl)
  (for/list ([body (in-list bodies)])
    (define body-type (rival-type body (map (curryr cons 'real) args)))
    (match body-type
      ['bool boolean-discretization]
      ['real (repl-real-discretization repl)]
      [#f (raise-user-error 'compile "Type error in function")])))

(define (normalize-function-name name)
  (if (string-prefix? name "ival-")
      (substring name 5)
      name))

(define (repl-real-discretization repl)
  (match (repl-precision repl)
    ['fp64 flonum-discretization]
    [`(bf ,n) (bf-discretization n)]))

(define (executions-iterations execs)
  (define iter 0)
  (define last #f)
  (for/list ([exec (in-vector execs)])
    (define id (execution-number exec))
    (when (and last (< id last))
      (set! iter (+ iter 1)))
    (set! last id)
    (cons iter exec)))

(define (write-table fn #:rows rows #:cols cols #:width [width 8])
  (for ([row (in-range rows)])
    (for ([col (in-range cols)])
      (display (~a (fn row col) #:width width #:align 'right)))
    (newline)))

(define (lookup-execution execs
                          #:iter [target-iter #f]
                          #:id [target-id #f]
                          #:name [target-name #f]
                          #:default [default ""]
                          #:value [value (lambda (_iter exec) exec)])
  (define entry
    (for/first ([exec (in-list execs)]
                #:when (and (or (not target-iter) (= (car exec) target-iter))
                            (or (not target-id) (= (execution-number (cdr exec)) target-id))
                            (or (not target-name) (= (execution-name (cdr exec)) target-name))))
      exec))
  (if entry
      (value (car entry) (cdr entry))
      default))

(struct repl ([precision #:mutable] context))

(define (make-repl [precision 'fp64])
  (repl precision (make-hash)))

(define (repl-compile repl args bodies)
  (rival-compile bodies args (create-discs args bodies repl)))

(define (repl-get-machine repl name)
  (if (symbol? name)
      (hash-ref (repl-context repl) name (lambda () (raise-user-error "Unknown function ~a" name)))
      (repl-compile repl '() (list name))))

(define (->bf x)
  (if (number? x)
      (bf x)
      x))

(define (repl-value->string val)
  (cond
    [(bigfloat? val) (bigfloat->string val)]
    [(number? val) (~r val)]
    [else (~a val)]))

(define (repl-precision-bits repl)
  (match (repl-precision repl)
    [`(bf ,n) n]
    ['fp64 53]))

(define (repl-apply repl machine vals)
  (with-handlers ([exn:rival:invalid? (const "Domain error")]
                  [exn:rival:unsamplable? (const "Could not evaluate")])
    (parameterize ([bf-precision (repl-precision-bits repl)])
      (rival-apply machine (list->vector (map ->bf vals))))))

(define (repl-save-machine! repl name args bodies)
  (hash-set! (repl-context repl) name (repl-compile repl args bodies)))

(define (check-args! name machine vals)
  (define args (vector->list (rival-machine-arguments machine)))
  (unless (= (length args) (length vals))
    (raise-user-error name "Expects ~a arguments" (length args))))

(define (write-explain machine)
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
        (lookup-execution execs*
                          #:iter iter
                          #:name 'adjust
                          #:default ""
                          #:value (lambda (_iter exec)
                                    (~r (* (execution-time exec) 1000) #:precision '(= 1))))]
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
        (lookup-execution execs*
                          #:id id
                          #:default ""
                          #:value (lambda (_iter exec)
                                    (normalize-function-name (~a (execution-name exec)))))]
       [(row col)
        #:when (= (modulo col 2) 1) ; precision
        (define id (+ (- row 3) num-args))
        (define iter (/ (- col 1) 2))
        (lookup-execution execs*
                          #:iter iter
                          #:id id
                          #:default ""
                          #:value (lambda (_iter exec) (execution-precision exec)))]
       [(row col)
        #:when (= (modulo col 2) 0) ; time
        (define id (+ (- row 3) num-args))
        (define iter (/ (- col 2) 2))
        (define value
          (lookup-execution execs*
                            #:iter iter
                            #:id id
                            #:default ""
                            #:value (lambda (_iter exec)
                                      (~r (* (execution-time exec) 1000) #:precision '(= 1)))))
        value]))))

(define (rival-repl p)
  (let/ec k
    (parameterize ([read-decimal-as-inexact #f]
                   [*rival-name-constants* #t])
      (define repl (make-repl))
      (when (terminal-port? p)
        (display "> "))
      (for ([cmd (in-port read p)])
        (with-handlers ([exn:fail:user? (lambda (e) (eprintf "ERROR ~a\n" (exn-message e)))])
          (match cmd
            [`(set precision fp64)
             (set-repl-precision! repl 'fp64)]
            [`(set precision ,(? integer? n))
             (when (< n 4)
               (raise-user-error 'set "Precision must be an integer greater than 3"))
             (set-repl-precision! repl (list 'bf n))]
            [`(define (,(? symbol? name) ,(? symbol? args) ...)
                ,bodies ...)
             (repl-save-machine! repl name args bodies)]
            [`(eval ,name ,(? (disjoin real? boolean?) vals) ...)
             (define machine (repl-get-machine repl name))
             (check-args! name machine vals)
             (define out (repl-apply repl machine vals))
             (if (string? out)
                 (displayln out)
                 (for ([val (in-vector out)])
                   (displayln (bigfloat->string val))))]
            [`(explain ,name ,(? (disjoin real? boolean?) vals) ...)
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
             (displayln "  (set precision <n>)                      Set working precision to n")
             (displayln "  (define (<name> <args> ...) <body> ...)  Define a named function")
             (displayln "  (eval <name> <vals> ...)                 Evaluate a named function")
             (displayln
              "  (explain <name> <vals> ...)          Show profile for evaluating a named function")
             (newline)
             (displayln "A closed expression can always be used in place of a named function.")]
            [(or '(exit) 'exit) (k)]
            [_ (printf "Unknown command ~a; use help for command list\n" cmd)]))
        (when (terminal-port? p)
          (display "> "))))
    (when (terminal-port? p)
      (displayln "exit"))))

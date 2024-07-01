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
         (rival-profile machine 'executions)
         (define start (current-inexact-milliseconds))
         (parameterize ([bf-precision precision])
           (vector-ref (rival-apply machine (list->vector (map bf vals))) 0))
         (define end (current-inexact-milliseconds))
         (define execs (rival-profile machine 'executions))
         (define num-instructions (rival-profile machine 'instructions))
         (define num-iterations (rival-profile machine 'iterations))
         (define num-args (vector-length (rival-machine-arguments machine)))
         (printf "Executed ~a instructions for ~a iterations:\n\n" num-instructions num-iterations)

         (define names (make-vector (+ num-instructions 1) #f))
         (define iters (make-vector (+ num-instructions 1) 0))
         (define times (build-vector (+ num-instructions 1) (lambda (i) (make-hash))))
         (define precs (build-vector num-instructions (lambda (i) (make-hash))))

         (for ([exec (in-vector execs)])
           (match-define (execution name number precision time) exec)
           (define number* (if (eq? name 'adjust) num-instructions (- number num-args)))
           (vector-set! names number* name)
           (define iter (vector-ref iters number*))
           (hash-set! (vector-ref times number*) iter time)
           (when (< number* num-instructions)
             (hash-set! (vector-ref precs number*) iter precision))
           (vector-set! iters number* (+ iter 1)))

         (for ([name (in-vector names)] [iters (in-vector iters)]
                                        [time-n (in-vector times)] [prec-n (in-vector precs)])
           (display (~a (normalize-function-name (~a name)) #:width 8))
           (for ([n (in-range iters)])
             (when (and (hash-has-key? time-n n) (hash-has-key? prec-n n))
               (display (~r (hash-ref prec-n n) #:min-width 8)))
               (display (~r (* (hash-ref time-n n) 1000) #:precision '(= 2) #:min-width 8)))
           (newline))

         (display "adjust  ")
         (define time-n (vector-ref times num-instructions))
         (display "        ")
         (display "        ")
         (for ([i (in-range (vector-ref iters num-instructions))])
           (display "        ")
           (display (~r (* (hash-ref time-n i) 1000) #:precision '(= 2) #:min-width 8)))

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

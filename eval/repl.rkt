#lang racket/base

(require racket/match racket/string)
(require (only-in math/private/bigfloat/mpfr bfcopy bigfloats-between bf-precision bigfloat->string bf))
(require "main.rkt" "machine.rkt")
(provide rival-repl)

(define (bf-discretization n)
  (discretization
   (lambda (x) (parameterize ([bf-precision n]) (bfcopy x)))
   (lambda (x y) (parameterize ([bf-precision n]) (bigfloats-between x y)))))

(define (fix-up-fpcore expr)
  (match expr
    [`PI '(PI)]
    [`E '(E)]
    [`(,op ,args ...) (list* op (map fix-up-fpcore args))]
    [_ expr]))

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
        [`(FPCore ,(? symbol? name) (,(? symbol? args) ...)
                  ,(and (or (? list?) (? symbol?) (? number?))
                        body))
         (hash-set! fns name
                    (rival-compile (list (fix-up-fpcore body)) args
                                   (list (bf-discretization precision))))]
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
    (newline))

#lang racket

(require json math/bigfloat)
(require "main.rkt" "infra/run-mathematica.rkt")

(define (read-from-string s)
  (read (open-input-string s)))

(define (my-rival-compile progs exprs)
  (rival-compile progs exprs (cons boolean-discretization (map (const flonum-discretization) (cdr progs)))))

(wolfram-log (current-error-port))

(define backends
  (list (list "Rival" my-rival-compile rival-apply)
        (list "Wolfram" wolfram-compile wolfram-apply)))

(define (compare-backends html? p)
  (for ([rec (in-port read-json p)] [i (in-naturals)])
    (define exprs (map read-from-string (hash-ref rec 'exprs)))
    (define vars (map read-from-string (hash-ref rec 'vars)))
    (unless (andmap symbol? vars)
      (raise 'time "Invalid variable list ~a" vars))
    (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))

    (define machines
      (for/list ([backend (in-list backends)])
        (match-define (list name b-compile b-app) backend)
        (b-compile exprs vars)))

    (define timess
      (for/list ([backend (in-list backends)] [machine (in-list machines)])
        (match-define (list name b-compile b-app) backend)
        (for/list ([pt (in-list (hash-ref rec 'points))])
          (define start-apply (current-inexact-milliseconds))
          (define status
            (with-handlers ([exn:rival:invalid? (const 'invalid)]
                            [exn:rival:unsamplable? (const 'unsamplable)])
              (b-app machine (list->vector (map bf pt)))
              'valid))
          (define apply-time (- (current-inexact-milliseconds) start-apply))
          (cons status apply-time))))

    (when html?
      (printf "<pre>\n"))
    (printf "~a:" (~a i #:align 'left #:min-width 3))
    (for ([times (in-list timess)] [backend (in-list backends)])
      (match-define (list name b-compile b-app) backend)
      (printf "~a: ~ams"
              (~a name #:align 'right #:min-width 8)
              (~r (apply + (map cdr times)) #:precision '(= 3))))
    (printf "\n"))

    (when html?
      (printf "</pre>\n")))

(module+ main
  (require racket/cmdline)
  (define html? #f)
  (command-line
   #:once-each
   [("--html") "Produce HTML output"
               (set! html? #t)]
   #:args ([points "infra/points.json"])
   (compare-backends html? (open-input-file points))))


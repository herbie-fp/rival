#lang racket


(require math/bigfloat)
(require math/bigfloat)

(require "./interval-evaluate.rkt")
(require "./run-mpfi.rkt")



(define (run-on-points port output rival-port point-count)
  (define read-res (read port))
  (when (equal? (modulo point-count 1000) 0)
      (display "Ran Rival on ")
      (display point-count)
      (displayln " points"))
  
  (when (not (equal? read-res eof))
    (match-define (list suite prog pt) read-res)
    (define str (prog->wolfram prog pt))
    (define rival-res
      (parameterize ([bf-precision 10000])
        (interval-evaluate (program-body prog) (program-variables prog) pt #f)))
    (writeln (list suite prog pt rival-res) rival-port)

    (run-on-points port output rival-port (+ point-count 1))))


(module+ main
  (command-line #:program "run-mpfi"
    #:args (points-file rival-file)
    (define rival-port (open-output-file rival-file #:exists 'replace))
    (run-on-points points-file rival-port)))

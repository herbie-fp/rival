#lang racket

(require math/bigfloat "../main.rkt")
(provide math-path wolfram-apply wolfram-compile time-expr-math)

(define function->wolfram
  (make-hash
    `((assert . Assert)
      (pow . Power)
      (+ . Plus)
      (- . Subtract)
      (/ . Divide)
      (sqrt . Sqrt)
      (* . Times)
      (fma . FMA)
      (hypot . Hypot)
      (exp . Exp)
      (log . Log)
      (log10 . Log10)
      (log2 . Log2)
      (cbrt . CubeRoot)
      (sin . Sin)
      (cos . Cos)
      (tan . Tan)
      (asin . ArcSin)
      (acos . ArcCos)
      (atan . ArcTan)
      (sinh . Sinh)
      (cosh . Cosh)
      (tanh . Tanh)
      (asinh . ArcSinh)
      (acosh . ArcCosh)
      (atanh . ArcTanh)
      (atan2 . ArcTan)
      (erf . Erf)
      (erfc . Erfc)
      (tgamma . Gamma)
      (lgamma . LogGamma)
      (ceil . Ceiling)
      (floor . Floor)
      (fmod . Mod)
      (remainder . QuotientRemainer)
      (fmax . Max)
      (fmin . Min)
      (truc . Truncate)
      (round . Round)
      (if . If)
      (< . LessThan)
      (> . GreaterThan)
      (<= . LessEqual)
      (>= . GreaterEqual)
      (== . Equal)
      (!= . NotEqual)
      (and . And)
      (or . Or)
      (not . Not)
      (neg . Minus)
      (fabs . Abs))))

;; exp2, expm1, 

(define (number->wolfram num)
  (format "Divide[~a, ~a]" (numerator num) (denominator num)))

(define (expr->wolfram expr)
  (match expr
    [(list op (app expr->wolfram args) ...)
     (define fn (hash-ref function->wolfram op))
     (format "checkReal[~a[~a]]" fn (string-join args ", "))]
    ['PI
     "Pi"]
    ['E
     "E"]
    [(? symbol?)
     (regexp-replace #rx"[*._-]" (symbol->string expr) "AWeirdSymbol")]
    [(? number?)
     (number->wolfram expr)]))

(define (program->wolfram exprs vars)
  (format "f[~a] := {~a}"
            (string-join (map (compose (curry format "~a_") expr->wolfram) vars) ", ")
            (string-join (map expr->wolfram exprs) ", ")))

(define (load-points port)
  (define points
    (for/list ([read-res (in-port read port)])
      read-res))
  (for/hash ([group (group-by second points)])
    (values (second (car group)) (map third group))))

(define math-path (find-executable-path "math"))

(struct wolfram-machine (exprs vars proc out in err) #:mutable)

(define headers
  (list
   "$MaxExtraPrecision=3100"
   "flConst[x_] := Rationalize[SetPrecision[x, 60]]"
   "FMA[x_, y_, z_] := x * y + z"
   "Hypot[x_, y_] := Sqrt[x*x + y*y]"
   "checkReal[a_] := If[Or[Internal`RealValuedNumericQ[a], BooleanQ[a]],a, Throw[\"domain-error\", BadValue]]"))

(define wolfram-log (make-parameter #f))

(define (wolfram-compile exprs vars)
  (define-values (process m-out m-in m-err)
    (subprocess #f #f #f math-path))

  (define buffer (make-bytes 65536 0))

  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when (wolfram-log) (apply fprintf (wolfram-log) fmt vs))
    (flush-output m-in))

  (for ([line (in-list headers)])
    (ffprintf "~a\n" line))
  (ffprintf "~a\n" (program->wolfram exprs vars))
  (ffprintf "Print[\"Rival\" <> \"Ready\"]\n")
  (let loop ([i 0])
    (define step (read-bytes-avail! buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (if (regexp-match #rx".*RivalReady.*In\\[[0-9]+\\].*" s)
        (eprintf "Mathematica started for ~a\n" exprs)
        (loop (+ i step))))

  (wolfram-machine exprs vars process m-out m-in m-err))

(define (wolfram-reset! machine)
  (subprocess-kill (wolfram-machine-proc machine) true)
  (match-define (wolfram-machine exprs vars proc in out err)
    (wolfram-compile (wolfram-machine-exprs machine)
                     (wolfram-machine-vars machine)))
  (set-wolfram-machine-proc! machine proc)
  (set-wolfram-machine-in! machine in)
  (set-wolfram-machine-out! machine out)
  (set-wolfram-machine-err! machine err))

(define (wolfram-shutdown! machine)
  (fprintf (wolfram-machine-in machine) "Exit[]\n")
  (when (wolfram-log) (fprintf (wolfram-log) "Exit[]\n"))
  (flush-output (wolfram-machine-in machine))
  (subprocess-wait (wolfram-machine-proc machine))
  (subprocess-status (wolfram-machine-proc machine)))

(define (wolfram-apply machine pt)
  (define buffer (make-bytes 65536 0))
  (define (ffprintf fmt . vs)
    (apply fprintf (wolfram-machine-in machine) fmt vs)
    (when (wolfram-log) (apply fprintf (wolfram-log) fmt vs))
    (flush-output (wolfram-machine-in machine)))

  (define start (current-inexact-milliseconds))
  (ffprintf "TimeConstrained[FullForm[N[f[~a], 20]], 1]\n"
            (string-join (map (compose number->wolfram bigfloat->rational) (vector->list pt)) ", "))
  (let loop ([i 0])
    (define step (read-bytes-avail!* buffer (wolfram-machine-out machine) i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (cond
      [(> (- (current-inexact-milliseconds) start) 2000.0)
       (eprintf "Killing and restarting Mathematica\n")
       (eprintf "~s\n" s)
       (wolfram-reset! machine)
       (raise (exn:rival:unsamplable "Freeze" pt))]
      [(string-contains? s "\nIn")
       (match (parse-output s)
         ['invalid
          (raise (exn:rival:invalid "Invalid input" pt))]
         ['memory
          (raise (exn:rival:unsamplable "Out of memory" pt))]
         ['timeout
          (raise (exn:rival:unsamplable "Timeout" pt))]
         ['unsamplable
          (raise (exn:rival:unsamplable "Indeterminate" pt))]
         ['unknown
          (raise (exn:rival:unsamplable "Unknown" pt))]
         [(regexp #rx"[0-9]+(\\.[0-9]+)?(`[0-9]*)?(\\*\\^q-?[0-9]+)?" (list s _))
          (parse-number s)])]
      [else
       (loop (+ i step))])))

(define (parse-output s)
  (define lines (string-split s "\n" #:repeat? #t))
  (with-handlers ([exn:misc:match? (λ (e)
    (newline)
    (printf "Could not parse results:\n")
    (pretty-print lines)
    (exit))])

    (match-lines lines)))

(define (match-lines lines)
  (match lines
    [(list
      (regexp #rx"In\\[[0-9]+\\]:= ")
      rest ...)
     (match-lines rest)]
    [(list
      rest ...
      (regexp #rx"In\\[[0-9]+\\]:= "))
     (match-lines rest)]
    [(list (regexp #rx"Out\\[[0-9]+\\]= \\$Aborted"))
     'timeout]
    [(list (regexp #rx"Out\\[[0-9]+\\]//FullForm= (.+)" (list _ x)))
     x]
    [(list
      (regexp #rx"Out\\[[0-9]+\\]//FullForm= ")
      " "
      (regexp #rx"> +[0-9-](.+)" (list _ x)))
     x]
    [(list
      "Throw::nocatch: Uncaught Throw[domain-error, BadValue] returned to top level."
      _ ...)
     'invalid]
    [(list
      "General::ovfl: Overflow occurred in computation."
      _ ...)
     'unsamplable]
    [(list
      "General::unfl: Underflow occurred in computation."
      _ ...)
     'unsamplable]
    [(list
      "General::nomem: "
      "   The current computation was aborted because there was insufficient memory"
      "    available to complete the computation."
      _ ...)
     (eprintf "Mathematica ran out of memory!\n")
     'memory]
    [(list
      _ ...
      (regexp #rx"Divide::indet: Indeterminate expression .*")
      _ ...)
     'unsamplable]
    [(list
      _ ...
      (regexp #rx"Divide::infy: Infinite expression .*")
      _ ...)
     'unsamplable]
    [(list
      "Divide::infy: Infinite expression "
      _ ...)
     'unsamplable]
    [(list
      _ ...
      (regexp #rx"Power::infy: Infinite expression .*")
      _ ...)
     'unsamplable]
    [(list
      _ ...
      (regexp #rx"Infinity::indet: Indeterminate expression .*")
      _ ...)
     'unsamplable]
    [(list
      _ ...
      "Infinity::indet: "
      _ ...
      (regexp #rx"Indeterminate expression .*")
      _ ...)
     'unsamplable]
    [(list
      _ ...
      (regexp #rx"ArcTan::indet: Indeterminate expression .*")
      _ ...)
     'unsamplable]
    [(list
      (regexp #rx"General::stop: Further output of .*")
      (regexp #rx" +will be suppressed during this calculation")
      _ ...)
     'unsamplable]
    [(list
      "N::meprec: Internal precision limit $MaxExtraPrecision = 3100."
      _ ...)
     'unknown]))

;; TODO: Use Timing[] for timing

(define (parse-number s) 1.0) ; fake

(define (read-from-string s)
  (read (open-input-string s)))

(define (time-expr-math rec)
  (cond
   [(not math-path)
    (list)]
   [else
    (call-with-output-file "mathematica.log" #:exists 'replace
      (λ (p)
        (define exprs (map read-from-string (hash-ref rec 'exprs)))
        (define vars (map read-from-string (hash-ref rec 'vars)))
        (unless (andmap symbol? vars)
          (raise 'time "Invalid variable list ~a" vars))
        (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))
        (define start-compile (current-inexact-milliseconds))
        (define machine (wolfram-compile exprs vars))
        (define compile-time (- (current-inexact-milliseconds) start-compile))

        (define times
          (for/list ([pt (in-list (hash-ref rec 'points))])
            (define start-apply (current-inexact-milliseconds))
            (define status
              (with-handlers ([exn:rival:invalid? (const 'invalid)]
                              [exn:rival:unsamplable? (const 'unsamplable)])
                (wolfram-apply machine (list->vector (map bf pt)))
                'valid))
            (define apply-time (- (current-inexact-milliseconds) start-apply))
            (cons status apply-time)))

        (cons (cons 'compile compile-time) times)))]))

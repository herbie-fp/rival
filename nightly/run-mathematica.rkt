#lang racket


(require math/bigfloat)
(require math/bigfloat)

(require "./interval-evaluate.rkt")
(require "./run-mpfi.rkt"


(define (set-var v)
  (*var-reprs* (cons (cons v (get-representation 'binary64)) (*var-reprs*))))


(define to-mathematica-function
  (make-hash
    `((pow . Power)
      (+ . Plus)
      (- . Subtract)
      (/ . Divide)
      (sqrt . Sqrt)
      (* . Times)
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

(define (transform-function func)
  (define beginning (string->symbol (first (string-split (symbol->string func) "."))))
  (cond
    [(hash-has-key? to-mathematica-function beginning)
     (hash-ref to-mathematica-function beginning)]
    [else 
      #f]))

(define (number->wolfram num)
  (define split (string-split (number->string num) "e"))
  (if
    (equal? (length split) 1)
    (first split)
    (string-append (first split) "*^" (string-replace (second split) "+" ""))))
    

(define (transform-wolfram expr hash)
  (cond
    [(list? expr)
     (define transformed-name (transform-function (first expr)))
     (define r (map (curryr transform-wolfram hash) (rest expr)))
     (if (andmap identity (cons transformed-name r))
         (format "~a[~a]" (transform-function (first expr))
                         (string-join
                           r
                           ", "))
        #f)]
    [(hash-has-key? hash expr)
     (hash-ref hash expr)]
    [(number? expr)
     (number->wolfram expr)]
    [(equal? expr 'PI.f64)
     "Pi"]
    [else
     (error "found constant or something")]))

(define (prog->wolfram prog point)
  (define hash (make-hash (map cons (program-variables prog) (map number->wolfram point))))
  (transform-wolfram (program-body prog) hash))

(define todo empty)

(define (run-on-points port output rival-port point-count)
  (define read-res (read port))
  (when (equal? (modulo point-count 1000) 0)
      (display "Converted ")
      (display point-count)
      (displayln " points")
      )

  (when (equal? read-res eof)
    (fprintf output "Null\n"))
  
  (when (not (equal? read-res eof))
    (match-define (list suite prog pt) read-res)
    (define str (prog->wolfram prog pt))
    (*var-reprs* '())
    (map set-var (program-variables prog))
    (when (and str (expr-supports? (program-body prog) 'ival))
    	  (define rival-res
         (parameterize ([bf-precision 10000])
                       (interval-evaluate (program-body prog) (program-variables prog) pt #f))))
	  (writeln (list suite prog pt rival-res) rival-port)
	  (write-todo str output))

    (run-on-points port output rival-port (+ point-count 1))))

(define (write-todo item output)
    (fprintf output "Print[\"\\\"\"],\n")
    (fprintf output "V := Catch[N[~a, 16], _SystemException],\n" item)
    (fprintf output "Print[ReturnIfReal[V]],\n")
    (fprintf output "Print[\"\\\"\"],\n"))
    

(define (run-mathematica script-file output-port)
  (define-values (process in out err) (subprocess output-port #f #f (find-executable-path "wolframscript") "-file" (build-path script-file)))
  (subprocess-wait process))

(module+ main
  (command-line #:program "run-mpfi"
    #:args (points-file script-file output-file rival-file)
    (define rival-port (open-output-file rival-file #:exists 'replace))
    (define script-port (open-output-file script-file #:exists 'replace))
    (displayln "#!/usr/bin/env wolframscript" script-port)
    (displayln "Block[{$MaxExtraPrecision = 500},{" script-port)
    (displayln "ReturnIfReal := Function[a, If[NumericQ[a], If[Internal`RealValuedNumericQ[a], a, \"domain-error\"], \"unsamplable\"]],\n" script-port)
    
    (run-on-points (open-input-file points-file) script-port rival-port 0)
    (displayln "}]" script-port)
    (flush-output script-port)
    (close-output-port script-port)

    (println "Running mathematica on converted points")
    (define output-port (open-output-file output-file #:exists 'replace))
    (run-mathematica script-file output-port)))
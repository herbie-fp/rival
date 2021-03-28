#lang racket

(require math/bigfloat xml racket/date)


(require biginterval)
(require "../main.rkt")
(require "format-mathematica.rkt")
(require "plot-example-cases.rkt")

(struct idata (mpfi-error-hash rival-error-hash may-error-mpfi-good rival-samplable mpfi-samplable))

(define (get-mpfi-left mpfi)
  (bf-list->bf (first mpfi)))

(define (get-mpfi-right mpfi)
  (bf-list->bf (second mpfi)))

(define (sum-benches bench-to accessor)
  (for/sum ([suite (hash-keys bench-to)])
    (accessor (hash-ref bench-to suite))))

(define (get-substrings-divisible str n)
  (cond
    [(equal? (string-length str) n)
     (list str)]
    [else
      (cons (substring str 0 n) (get-substrings-divisible (substring str n) n))]))

(define (get-substrings str n)
  (define head (substring str 0 (modulo (string-length str) n)))
  (define tail (substring str (modulo (string-length str) n)))
  (cond
    [(equal? (string-length tail) 0)
     (list head)]
    [(equal? (string-length head) 0)
     (get-substrings-divisible tail n)]
    [else
     (cons head (get-substrings-divisible tail n))]))
     

(define (insert-thinspaces string space)
  (string-join (get-substrings string 3) space))

(define (latex-format-item item)
  (cond
    [(exact-integer? item)
     (insert-thinspaces (format "~a" item) "\\thinspace")]
    [else (format "~a" item)]))

(define (html-format-number item)
  (insert-thinspaces (~a item) "\u2009"))

(define (latex-format-label label)
  (string-replace 
   (string-replace label "MAYBE" "$[\\bot, \\top]$")
   "YES" "[\\top, \\top]"))

(define (html-format-label label)
  (string-replace 
   (string-replace label "MAYBE" "[\u22a5, \u22a4]")
   "YES" "[\u22a4, \u22a4]"))

(define (max-num data)
  (apply max
    (map (lambda (d) (if (number? d) d -inf.0)) data)))

(define (min-num data)
  (apply min
    (map (lambda (d) (if (number? d) d +inf.0)) data)))


(define (get-bold-index data good)
  (define processed (map (lambda (d) (if (number? d) (exact->inexact d) d)) data))
  (cond
    [(equal? good 'none)
     (length processed)]
    [(or (equal? good 'min) (equal? good 'max))
     (define best ((if (equal? good 'max) max-num min-num) processed))
     (define after (member best processed))
     
     (if (and (not (equal? #f after)) (equal? #f (member best (rest after))))
         (index-of processed best)
	 (length processed))]))

(define (make-latex-bold str)
  (string-append "\\textbf{" str "}"))

(define (bold-correct-item data good)
  (define bold-index (get-bold-index data good))
  (define strings (map latex-format-item data))
  (define-values (before after) (split-at strings bold-index))
  (define modified-after (if (> (length after) 0)
                             (cons (make-latex-bold (first after)) (rest after))
			     empty))
  (append before modified-after))

(define (make-latex-row data #:good [good 'none])
  (match-define (list label elts ...) data)
  (format "~a & ~a \\\\\n"
          (latex-format-label label)
          (string-join (bold-correct-item elts good) " & ")))

(define (make-html-row data #:good [good 'none])
  (match-define (list label elts ...) data)
  `(tr (th ,(html-format-label label)) ,@(for/list ([elt elts]) `(td ,(html-format-number elt)))))

(define (make-both-row port data #:good [good 'none])
  (displayln (make-latex-row data #:good good) port)
  (make-html-row data #:good good))

#;(define (output-data bench-to-mdata bench-to-idata output-port)
  ;;(displayln "\\begin{tabular}{r|rrrr}" output-port)

  (define total-points
          (+ (sum-benches bench-to-mdata mdata-mathematica-unsamplable)
	     (sum-benches bench-to-mdata mdata-mathematica-samplable)))
  (define mpfi-supported
          (+ (sum-benches bench-to-idata (lambda (d) (hash-ref (idata-mpfi-error-hash d) 'f)))
	     (sum-benches bench-to-idata (lambda (d) (hash-ref (idata-mpfi-error-hash d) 'o)))))

  (define rival-invalid-guarantee (sum-benches bench-to-mdata (lambda (d) (hash-ref (mdata-rival-error-hash d) 't))))
  (define rival-invalid-unsure (sum-benches bench-to-mdata (lambda (d) (hash-ref (mdata-rival-error-hash d) 'o))))
  (define mpfi-invalid (sum-benches bench-to-idata (lambda (d) (hash-ref (idata-mpfi-error-hash d) 'o))))
  (define mathematica-unsamplable (- (sum-benches bench-to-mdata mdata-mathematica-unsamplable) (sum-benches bench-to-mdata mdata-mathematica-error)))
  (define mathematica-invalid-guarantee (sum-benches bench-to-mdata mdata-mathematica-error))

  (define rival-movability-stuck (sum-benches bench-to-mdata mdata-rival-movability))
  (define rival-unsamplable-possible (sum-benches bench-to-mdata mdata-rival-possible))
  (define mpfi-unsamplable (- mpfi-supported (sum-benches bench-to-idata idata-mpfi-samplable) mpfi-invalid))

  (define rival-samplable (sum-benches bench-to-mdata mdata-rival-samplable))
  (define mathematica-samplable (sum-benches bench-to-mdata mdata-mathematica-samplable))
  (define-values (nightly-slack-process _in _out _err)
    (subprocess #f #f #f (find-executable-path "nightly-results") "best"
                (cond
                  [(> rival-samplable mathematica-samplable)
                   "Rival"]
                  [(< rival-samplable mathematica-samplable)
                   "Mathematica"]
                  [else "Tied"])))
  (subprocess-wait nightly-slack-process)
  
  
  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (link ([rel "stylesheet"] [href "index.css"]))
      (title "Rival data for " ,(date->string (current-date))))
     (body
      (h1  "Rival data for " ,(date->string (current-date)))
      (table
       (tr (th) (th "Rival") (th "MPFI") (th "Mathematica"))
       ,(make-html-row (list "Samplable" rival-samplable
                                        (sum-benches bench-to-idata idata-mpfi-samplable)
                                        mathematica-samplable) #:good 'max)
       ,(make-html-row
        (list "Unsupported"
              0
		          (- total-points mpfi-supported)
		          0))
       ,(make-html-row
        (list "Total Invalid"
        	     (+ rival-invalid-guarantee rival-invalid-unsure)
		           mpfi-invalid
		           mathematica-invalid-guarantee) #:good 'none)

       ,(make-html-row
               (list "Invalid YES"
	       	     rival-invalid-guarantee
		     0
		     mathematica-invalid-guarantee) #:good 'max)

       ,(make-html-row
                (list "Invalid MAYBE"
		      rival-invalid-unsure
		      mpfi-invalid
		      0) #:good 'min)

      ,(make-html-row
       		(list "Total Stuck"
		      (+ rival-movability-stuck rival-unsamplable-possible)
		      mpfi-unsamplable
		      mathematica-unsamplable) #:good 'none)

      ,(make-html-row
       		(list "Stuck YES"
               		      rival-movability-stuck
                       		      0
                               		      0) #:good 'max)

      ,(make-html-row
       		(list "Stuck MAYBE"
                               rival-unsamplable-possible
                               mpfi-unsamplable
                               mathematica-unsamplable) #:good 'min)

      )
     ))
  output-port
  )

  

  #;(displayln (make-html-row
                (list "No Error"
		(sum-benches bench-to-mdata (lambda (d) (hash-ref (mdata-rival-error-hash d) 'f)))
		(sum-benches bench-to-idata (lambda (d) (hash-ref (idata-mpfi-error-hash d) 'f)))	
		(sum-benches bench-to-mdata mdata-mathematica-samplable))) output-port)	     	
)

  ;(displayln "\\end{tabular}" output-port)
(define (is-nan? bigfloat)
  (equal? bigfloat '+nan.bf))

(define (output-var name val port [comment ""])
  (define comment-string
    (if (equal? comment "")
        ""
        (format "% ~a" comment)))
  (fprintf port "\\newcommand{\\~a}{~a\\xspace}~a\n" name val comment-string))


(define (round1 num)
  (~r num #:precision `(= 1)))

(define (output-percent proportion)
  (format "~a\\%"
          (round1 (* 100 proportion))))

(define (output-data tag points output sampled-chart-file bad-result-chart-file)
  (define overall-mathematica-timeout 0)
  (define overall-mathematica-memory 0)
  (define overall-mathematica-crash 0)
  
  (define total-count 0)
  (define rival-differs 0)
  (define rival-inf 0)
  (define rival-unknown 0)
  (define rival-errors 0)
  (define rival-immovable 0)
  (define mathematica-unsamplable 0)
  (define mathematica-domain-error 0)
  (define mathematica-memory 0)
  (define mathematica-unknown 0)
  (define mathematica-timeout 0)
  (define mathematica-crash 0)

  (define total-rival-errors 0)
  (define total-rival-sampled 0)
  (define total-rival-immovable 0)
  (define total-rival-unknown 0)
  (define total-mathematica-sampled 0)

  (for ([example points])
    (define mathematica-res (list-ref (list-ref example 4) 1))
    (cond [(equal? mathematica-res 'timeout)
           (set! overall-mathematica-timeout (add1 overall-mathematica-timeout))]
          [(equal? mathematica-res 'memory)
           (set! overall-mathematica-memory (add1 overall-mathematica-memory))]
          [(equal? mathematica-res 'crash)
           (set! overall-mathematica-crash (add1 overall-mathematica-crash))]))
  
  (for ([example points] #:when (member tag (list-ref example 5)))
    (define rival-res (list-ref example 3))
    (set! total-count (+ 1 total-count))
    (define mathematica-res (list-ref (list-ref example 4) 1))
    (when (number? mathematica-res)
      (set! total-mathematica-sampled (add1 total-mathematica-sampled)))
    (cond
      [(number? (list-ref rival-res 1))
       (set! total-rival-sampled (add1 total-rival-sampled))]
      [(list-ref rival-res 3)
       (set! total-rival-errors (add1 total-rival-errors))]
      [(list-ref rival-res 5)
       (set! total-rival-immovable (add1 total-rival-immovable))]
      [else
       (set! total-rival-unknown (add1 total-rival-unknown))])
    (cond
      [(number? mathematica-res)
       (cond
         [(and (number? (list-ref rival-res 1)) (infinite? (list-ref rival-res 1)))
          (set! rival-inf (add1 rival-inf))]
         [(number? (list-ref rival-res 1))
          (set! rival-differs (add1 rival-differs))])]
      [(equal? mathematica-res 'invalid)
       (set! mathematica-domain-error (add1 mathematica-domain-error))]
      [(equal? mathematica-res 'unsamplable)
       (set! mathematica-unsamplable (add1 mathematica-unsamplable))]
      [(equal? mathematica-res 'unknown)
       (set! mathematica-unknown (add1 mathematica-unknown))]
      [(equal? mathematica-res 'memory)
       (set! mathematica-memory (add1 mathematica-memory))]
      [(equal? mathematica-res 'timeout)
       (set! mathematica-timeout (add1 mathematica-timeout))]
      [(equal? mathematica-res 'crash)
       (set! mathematica-crash (add1 mathematica-crash))]
      [else (error "unknown mathematica value")]))

  (output-var "overallmathematicatimeoutormemory" (+ overall-mathematica-memory overall-mathematica-timeout overall-mathematica-crash) output)
  (output-var "overallmathematicacrash" overall-mathematica-crash output)  
  (output-var "overallallpoints" (length points) output)
  (output-var "overallrivalmathematicaagree" (- (length points) total-count) output)
  
  (output-var "totalrivalsamplesorerror" (+ total-rival-sampled total-rival-errors) output  "in all points where rival and mathematica disagree")
  (output-var "totalmathematicasamplesorerror" (+ total-mathematica-sampled mathematica-domain-error) output)
  (output-var "totalmathematicarivalmismatch" total-count output)
  (output-var "totalmathematicasamples" total-mathematica-sampled output)
  (output-var "totalrivalsamples" total-rival-sampled output)
  (output-var "totaldifferentnumbers" rival-differs output)
  (output-var "totalmathematicasamplablerivalinfinite" rival-inf output)
  (output-var "totalmathematicasamplablerivalunknown" total-rival-unknown output)
  (output-var "totalmathematicasamplablerivalerrors" total-rival-errors output)
  (output-var "totalmathematicasamplablerivalunsamplable" total-rival-immovable output)

  (output-var "totalrivalsamplablemathematicaunsamplable" mathematica-unsamplable output)
  (output-var "totalrivalsamplablemathematicadomainerror" mathematica-domain-error output)
  (output-var "totalrivalsamplablemathematicamemory" mathematica-memory output)
  (output-var "totalrivalsamplablemathematicaunknown" mathematica-unknown output)

  (when (not (equal? sampled-chart-file ""))
        (draw-chart (list total-mathematica-sampled mathematica-domain-error mathematica-unsamplable
                          mathematica-unknown (+ mathematica-crash mathematica-memory mathematica-timeout))
                    (list total-rival-sampled total-rival-errors total-rival-immovable total-rival-unknown)
                    sampled-chart-file))

  #;(when (not (equal? bad-result-chart-file ""))
        (draw-bad-result-chart (list mathematica-unsamplable mathematica-unknown mathematica-memory)
                               (list total-rival-immovable total-rival-unknown) bad-result-chart-file)))

(define (run-on-points port bench-to-idata sofar)
  (define read-res (read port))

  (when (equal? (modulo sofar 1000) 0)
        (printf "Processed ~a MPFI points\n" sofar))
  
  (cond
    [(not (equal? read-res eof))
     (match-define (list suite prog pt rival-res mpfi-res) read-res)

     (when (not (hash-has-key? bench-to-idata suite))
           (hash-set! bench-to-idata suite (idata (make-hash (list (cons 'f 0) (cons 'o 0)))
                                                  (make-hash (list (cons 't 0) (cons 'f 0) (cons 'o 0)))
                                                  0 0 0)))
     (define data (hash-ref bench-to-idata suite))
     
     (define mpfi-hash (idata-mpfi-error-hash data))
     (define rival-hash (idata-rival-error-hash data))
     
     (define has-nan? (or (is-nan? (first mpfi-res)) (is-nan? (second mpfi-res))))
     
     (if has-nan?
         (hash-update! mpfi-hash 'o (lambda (a) (+ a 1)))
         (hash-update! mpfi-hash 'f (lambda (a) (+ a 1))))

     (define rival-no-error #f)
     (define may-error-mpfi-good (idata-may-error-mpfi-good data))
     (cond
       [(equal? (vector-ref rival-res 4) #t)
        (hash-update! rival-hash 't (lambda (a) (+ a 1)))]
       [(equal? (vector-ref rival-res 3) #f)
        (set! rival-no-error #t)
        (hash-update! rival-hash 'f (lambda (a) (+ a 1)))]
       [else
        (when (not has-nan?)
              (set! may-error-mpfi-good (+ may-error-mpfi-good 1)))
        (hash-update! rival-hash 'o (lambda (a) (+ a 1)))])

     (define rival-samplable
       (if (and rival-no-error (samplable? (get-low rival-res) (get-hi rival-res)))
          (+ (idata-rival-samplable data) 1)
          (idata-rival-samplable data)))

     (define mpfi-samplable
       (if (and (not has-nan?) (samplable? (get-mpfi-left mpfi-res) (get-mpfi-right mpfi-res)))
          (+ (idata-mpfi-samplable data) 1)
          (idata-mpfi-samplable data)))

     (define new-data
       (struct-copy idata data
                                 [may-error-mpfi-good may-error-mpfi-good]
                                 [rival-samplable rival-samplable]
                                 [mpfi-samplable mpfi-samplable]))
     (hash-set! bench-to-idata suite new-data)
     (run-on-points port
                    bench-to-idata
                    (+ sofar 1))]
    [else
     bench-to-idata]))

(module+ main
  (command-line #:program "report"
    #:args (mpfi-results-file mathematica-results-file rival-results-file
            output-file examples-file macros-file sampled-plot-file bad-result-plot-file)
    (define results (collect-mathematica (open-input-file mathematica-results-file)
                                         (open-input-file rival-results-file)
                                         (open-output-file examples-file #:exists 'replace)
                                         0))
    #;(output-data (collect-mathematica (open-input-file mathematica-results-file)
                                      (open-input-file rival-results-file)
                                      (make-hash) 0
                                      (open-output-file examples-file #:exists 'replace)
                                      (open-output-file one-fails-file #:exists 'replace))
                 (run-on-points (open-input-file mpfi-results-file) (make-hash) 0)
		 (open-output-file output-file #:exists 'replace))
    (output-data 'result-mismatch results (open-output-file macros-file #:exists 'replace)
                                 "" "")
    (output-data 'hard-point results (open-output-nowhere)
                                 sampled-plot-file bad-result-plot-file)))

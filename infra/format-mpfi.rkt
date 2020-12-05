#lang racket

(require math/bigfloat xml racket/date)


(require biginterval)
(require rival)
(require "format-mathematica.rkt")

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

(define (html-format-item item)
  (cond
    [(exact-integer? item)
     (insert-thinspaces (format "~a" item) " ")]
    [else (format "~a" item)]))

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
  (string-append (string-join (bold-correct-item data good) " & ") " \\\\\n"))

(define (make-html-row data #:good [good 'none])
  `(tr
    ,(map (lambda (element) `(td ,element)) (map html-format-item data))))
  

(define (output-data bench-to-mdata bench-to-idata output-port)
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
  
  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (link ([rel "stylesheet"] [href "index.css"]))
      (title "Rival evaluation for " ,(date->string (current-date))))
     (body
      (h1  "Regraph evaluation for " ,(date->string (current-date)))
      (table
       ,(make-html-row (list "" "Rival" "MPFI" "Mathematica"))
       ,(make-html-row (list "Samplable" (sum-benches bench-to-mdata mdata-rival-samplable)                                        
                                        (sum-benches bench-to-idata idata-mpfi-samplable)
                                        (sum-benches bench-to-mdata mdata-mathematica-samplable)) #:good 'max)
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
               (list "Invalid $[\u22a5, \u22a5]$"
	       	     rival-invalid-guarantee
		     0
		     mathematica-invalid-guarantee) #:good 'max)

       ,(make-html-row
                (list "Invalid $[\u2284, \u22a5]$"
		      rival-invalid-unsure
		      mpfi-invalid
		      0) #:good 'min)

      ,(make-html-row
       		(list "Total Stuck"
		      (+ rival-movability-stuck rival-unsamplable-possible)
		      mpfi-unsamplable
		      mathematica-unsamplable) #:good 'none)

      ,(make-html-row
       		(list "Stuck $[\\top, \\top]$"
               		      rival-movability-stuck
                       		      0
                               		      0) #:good 'max)

      ,(make-html-row
       		(list "Stuck $[\\bot, \\top]$"
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
  (command-line #:program "run-mpfi"
    #:args (mathematica-results-file rival-results-file mpfi-results-file output-file)
    (output-data (collect-mathematica (open-input-file mathematica-results-file) (open-input-file rival-results-file) (make-hash) 0)
                 (run-on-points (open-input-file mpfi-results-file) (make-hash) 0)
		 (open-output-file output-file #:exists 'replace))))
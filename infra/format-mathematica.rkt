#lang racket

(require biginterval)


(require math/bigfloat math/flonum)
(require "../main.rkt")

(provide (struct-out mdata) collect-mathematica)
(provide samplable? get-low get-hi bf-list->bf)

;; samplable + unsamplable is total points
(struct mdata (rival-error-hash rival-samplable rival-movability rival-possible mathematica-samplable mathematica-unsamplable mathematica-error))

(define (samplable? left right)
  (define <-bf bigfloat->flonum)
  (define lo (<-bf left))
  (define hi (<-bf right))
  (if (or (equal? lo hi) (and (number? lo) (= lo hi)))
      lo
      false))

(define (equal-double? left right)
  (equal? (bigfloat->flonum (bf left)) (bigfloat->flonum (bf right))))

(define (within-one-ulp? left right)
  (define left-bf (bf left))
  (define right-bf (bf right))
  (define l-ordinal (flonum->ordinal (bigfloat->flonum left-bf)))
  (define r-ordinal (flonum->ordinal (bigfloat->flonum right-bf)))
  (<= (abs (- l-ordinal r-ordinal)) 1))

(define (get-endpoint-val endpoint)
  (bf-list->bf (vector-ref endpoint 1)))

(define (get-low ival)
  (get-endpoint-val (vector-ref ival 1)))
(define (get-hi ival)
  (get-endpoint-val (vector-ref ival 2)))


(define (is-nan? bigfloat)
  (equal? bigfloat '+nan.bf))

(define (bf-list->bf bf-list)
  (cond
    [(is-nan? bf-list)
     +nan.bf]
    [(equal? bf-list '0.bf)
     0.bf]
    [(equal? bf-list '-0.bf)
     0.bf]
    [(equal? bf-list '+inf.bf)
     +inf.bf]
    [(equal? bf-list '-inf.bf)
     -inf.bf]
    [else
     (bf (second bf-list))]))

(define (mathematica-domain-error? point-str)
  (equal? point-str 'invalid))


#;(define (mathematica-number? point-str)
  (define strings (string-split point-str "\n"))
  (cond [(equal? (length strings) 1)
         (define parts (string-split (first strings) (regexp "(\\*\\^)|(`+)")))
	 (and (< (length parts) 4)
	      (andmap string->number parts))]
	[else #f]))
      
(define (mathematica-samplable? point)
  (number? point))

;; mathematica-result is one of ('invalid 'memory 'unsamplable 'unknown) or a number
(define (results-match? mathematica-result rival-val rival-samplable? rival-no-error? rival-immovable?)
  (cond
    [(not (or (symbol? mathematica-result) (number? mathematica-result)))
     (error "weird mathematica output " mathematica-result)]
    [(and rival-samplable? (number? mathematica-result))
     (define within? (within-one-ulp? mathematica-result rival-val))
     within?]
    [(and rival-samplable? (not (number? mathematica-result)))
     false]
    [else
     (not (number? mathematica-result))]))
     

(define (is-immovable? rival-res)
  (define left-e (vector-ref rival-res 1))
  (define right-e (vector-ref rival-res 2))
  (define left-val (get-low rival-res))
  (define right-val (get-hi rival-res))
  (define left-i (vector-ref left-e 2))
  (define right-i (vector-ref right-e 2))
  (or (and left-i right-i) (and (bfinfinite? left-val) left-i) (and (bfinfinite? right-val) right-i)))
  

(define (collect-mathematica port rival-port bench-to-mdata sofar examples-port)
  (define read-res (read port))

  (when (equal? (modulo sofar 1000) 0)
        (printf "Processed ~a points\n" sofar))
  
  (cond
    [(not (equal? read-res eof))
     (match-define (list mprog mpt mathematica-time mathematica-result) read-res)
     (match-define (list suite prog pt rival-res) (read rival-port))
     (when (not (and (equal? prog mprog) (equal? pt mpt)))
       (error "Mathematica and Rival results not parallel" prog mprog pt mpt))

     (when (not (hash-has-key? bench-to-mdata suite))
           (hash-set! bench-to-mdata suite (mdata (make-hash (list (cons 't 0) (cons 'f 0) (cons 'o 0)))
                                                  0 0 0 0 0 0)))
     (define data (hash-ref bench-to-mdata suite))
     (define rival-hash (mdata-rival-error-hash data))

     (cond
       [(equal? (vector-ref rival-res 4) #t)
        (hash-update! rival-hash 't (lambda (a) (+ a 1)))]
       [(equal? (vector-ref rival-res 3) #f)
        (hash-update! rival-hash 'f (lambda (a) (+ a 1)))]
       [else
        (hash-update! rival-hash 'o (lambda (a) (+ a 1)))])

     (define rival-no-error (equal? (vector-ref rival-res 3) #f))
     
     (define is-samplable (and rival-no-error (samplable? (get-low rival-res) (get-hi rival-res))))
     (define rival-val (samplable? (get-low rival-res) (get-hi rival-res)))
     (define is-immovable (and (not is-samplable) rival-no-error (is-immovable? rival-res)))

     (match-define (mdata rival-error-hash rival-samplable rival-movability rival-possible mathematica-samplable mathematica-unsamplable mathematica-error) data)

     (define m-samplable? (mathematica-samplable? mathematica-result))
     (define m-error? (mathematica-domain-error? mathematica-result))

     (when (not (results-match? mathematica-result rival-val is-samplable rival-no-error is-immovable))
       (when (and (number? rival-val) (number? mathematica-result))
         (writeln (list suite prog pt (list "rival:" rival-val "error:" (not rival-no-error) "immovable:" is-immovable)
                                        (list "mathematica:" mathematica-result))))
       (writeln (list suite prog pt (list "rival:" rival-val "error:" (not rival-no-error) "immovable:" is-immovable)
                      (list "mathematica:" mathematica-result)) examples-port))
	   
     (define new-data
       (struct-copy mdata data
                          [rival-samplable (if is-samplable (+ rival-samplable 1) rival-samplable)]
			  [rival-movability (if is-immovable (+ rival-movability 1) rival-movability)]
			  [rival-possible (if (and rival-no-error (not is-samplable) (not is-immovable))
			                      (+ rival-possible 1) rival-possible)]
			  [mathematica-samplable (if m-samplable?
			                             (+ mathematica-samplable 1) mathematica-samplable)]
			  [mathematica-unsamplable (if (not m-samplable?)
			                               (+ mathematica-unsamplable 1)
						       mathematica-unsamplable)]
		    [mathematica-error (if m-error? (+ mathematica-error 1) mathematica-error)]))
     (hash-set! bench-to-mdata suite new-data)
     (collect-mathematica port
		    rival-port
        bench-to-mdata
        (+ sofar 1)
        examples-port)]
    [else
     bench-to-mdata]))

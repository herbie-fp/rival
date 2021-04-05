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
  

(define (collect-mathematica port rival-port examples-port sofar)
  (define read-res (read port))

  (when (equal? (modulo sofar 1000) 0)
        (printf "Processed ~a points\n" sofar))
  (define limit +inf.0)
  (cond
    [(> sofar limit)
     empty]
    [(not (equal? read-res eof))
     (match-define (list mprog mpt mathematica-time mathematica-result) read-res)
     (match-define (list suite prog pt rival-res) (read rival-port))
     (when (not (and (equal? prog mprog) (equal? pt mpt)))
       (error "Mathematica and Rival results not parallel" prog mprog pt mpt))
     
     (define rival-no-error (equal? (vector-ref rival-res 3) #f))
     
     (define is-samplable (and rival-no-error (samplable? (get-low rival-res) (get-hi rival-res))))
     (define rival-val
       (if is-samplable (samplable? (get-low rival-res) (get-hi rival-res)) #f))
     (define is-immovable (and (not is-samplable) rival-no-error (is-immovable? rival-res)))

     (define m-samplable? (mathematica-samplable? mathematica-result))
     (define m-error? (mathematica-domain-error? mathematica-result))

     (define tags (list 'any))
     (when (not (results-match? mathematica-result rival-val is-samplable rival-no-error is-immovable))
       (set! tags (cons 'result-mismatch tags)))
     (when (or (not is-samplable) (not (number? mathematica-result)))
       (set! tags (cons 'hard-point tags)))

     (define result
       (list suite prog pt (list "rival:" rival-val "error:" (not rival-no-error) "immovable:" is-immovable)
                      (list "mathematica:" mathematica-result) tags))

     (when (member 'hard-point tags)
           (writeln result examples-port))
     
     (cons result
           (collect-mathematica port
                                rival-port
                                examples-port
                                (+ sofar 1)))]
    [else
     empty]))

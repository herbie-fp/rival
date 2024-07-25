#lang racket

(require racket/math math/base math/flonum math/bigfloat racket/random profile)
(require json)
(require "main.rkt" "test.rkt" "profile.rkt"
         "eval/machine.rkt" ; for accessing iteration number of machine
         "infra/run-sollya.rkt" "infra/run-baseline.rkt")

(define sample-vals (make-parameter 5000))
(define *sampling-timeout* (make-parameter 20.0)) ; this parameter is used for plots generation

(define (time-operation ival-fn bf-fn itypes otype)
  (define n
    (if (set-member? slow-tests ival-fn)
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (define ivtime 0.0)
  (define bftime 0.0)
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (sample-interval itype)))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (define dt1 (- (current-inexact-milliseconds) start))
    (set! ivtime (+ ivtime dt1))
    (define start2 (current-inexact-milliseconds))
    (apply bf-fn (map ival-lo args))
    (apply bf-fn (map ival-hi args))
    (define dt2 (- (current-inexact-milliseconds) start2))
    (set! bftime (+ bftime dt2)))
  (values (* 1000 (/ ivtime n)) (* 1000 (/ bftime n))))

(define (read-from-string s)
  (read (open-input-string s)))

(define (time-expr rec timeline)
  (define exprs (map read-from-string (hash-ref rec 'exprs)))
  (define vars (map read-from-string (hash-ref rec 'vars)))
  (unless (andmap symbol? vars)
    (raise 'time "Invalid variable list ~a" vars))
  (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))
  (define discs (cons boolean-discretization (map (const flonum-discretization) (cdr exprs))))

  ; Rival machine
  (define start-compile (current-inexact-milliseconds))
  (define rival-machine (rival-compile exprs vars discs))
  (define compile-time (- (current-inexact-milliseconds) start-compile))

  ; Baseline and Sollya machines
  (define baseline-machine (baseline-compile exprs vars discs))
  (define sollya-machine (with-handlers ([exn:fail? (λ (e)
                                                      (printf "Sollya didn't compile")
                                                      (printf "~a\n" e)
                                                      #f)])
                           (sollya-compile exprs vars 53))) ; prec=53 is an imitation of flonum

  (define times
    (for/list ([pt (in-list (hash-ref rec 'points))])
      ; Rival execution
      (define rival-start-apply (current-inexact-milliseconds))
      (match-define (list rival-status rival-exs)
        (parameterize ([*rival-max-precision* 32256])
          (with-handlers ([exn:rival:invalid? (λ (e) (list 'invalid #f))]
                          [exn:rival:unsamplable? (λ (e) (list 'unsamplable #f))])
            (define exs (vector-ref (rival-apply rival-machine (list->vector (map bf pt))) 1))
            (list 'valid exs))))
      (define rival-apply-time (- (current-inexact-milliseconds) rival-start-apply))
      (define rival-iter (rival-machine-iteration rival-machine))
      
      (define rival-executions (rival-profile rival-machine 'executions))
      (for ([execution (in-vector rival-executions)])
        (define name (symbol->string (execution-name execution)))
        (define precision (execution-precision execution))
        (when (equal? rival-status 'valid)
          (timeline-push! timeline 'mixsample-rival-valid (list (execution-time execution) name precision)))
        (timeline-push! timeline 'mixsample-rival-all (list (execution-time execution) name precision)))

      ; Baseline execution (we assume that baseline can not crash)
      (define baseline-start-apply (current-inexact-milliseconds))
      (match-define (list baseline-status baseline-exs)
        (parameterize ([*rival-max-precision* 32256])
          (with-handlers ([exn:rival:invalid? (λ (e) (list 'invalid #f))]
                          [exn:rival:unsamplable? (λ (e) (list 'unsamplable #f))])
            (define exs (vector-ref (baseline-apply baseline-machine (list->vector (map bf pt)) #:timeout (*sampling-timeout*)) 1))
            (list 'valid exs))))
      (define baseline-apply-time (- (current-inexact-milliseconds) baseline-start-apply))
      
      (define baseline-executions (baseline-profile baseline-machine 'executions))
      (for ([execution (in-vector baseline-executions)])
        (define name (symbol->string (execution-name execution)))
        (define precision (execution-precision execution))
        (when (equal? baseline-status 'valid)
          (timeline-push! timeline 'mixsample-baseline-valid (list (execution-time execution) name precision)))
        (timeline-push! timeline 'mixsample-baseline-all (list (execution-time execution) name precision)))

      ; Sollya execution
      (when (and (and rival-machine baseline-machine sollya-machine)
                 (equal? rival-status 'valid)
                 (equal? rival-status 'unsamplable))
        
        (define sollya-apply-time 0.0)
        (match-define (list sollya-status sollya-exs)
          (match sollya-machine
            [#f (list #f #f)] ; if sollya machine is not working for this benchmark
            [else
             (with-handlers ([exn:fail? (λ (e)
                                          (printf "Sollya failed")
                                          (printf "~a\n" e)
                                          (sollya-kill sollya-machine)
                                          (set! sollya-machine #f)
                                          (list #f #f))])
               (match-define (list internal-time external-time exs status)
                 (sollya-apply sollya-machine pt #:timeout (*sampling-timeout*)))
               (set! sollya-apply-time external-time)
               (list status exs))]))

        ; When all the machines have compiled and work - write the results to outcomes
        (point-bucketing
         timeline
         rival-status rival-apply-time rival-exs
         baseline-status baseline-apply-time baseline-exs
         sollya-status sollya-apply-time sollya-exs
         rival-iter))
        
      (cons rival-status rival-apply-time)))

  ; Zombie process
  (when sollya-machine
    (sollya-kill sollya-machine))
  
  (cons (cons 'compile compile-time) times))


(define (time-exprs data)
  (define times
    (for/hash ([group (in-list (group-by car data))])
      (values (caar group) (map cdr group))))
 
  (list (/ (car (hash-ref times 'compile)) 1000)
         (length (hash-ref times 'valid '()))
         (/ (apply + (hash-ref times 'valid '())) 1000)
         (length (hash-ref times 'invalid '()))
         (/ (apply + (hash-ref times 'invalid '())) 1000)
         (length (hash-ref times 'unsamplable '()))
         (/ (apply + (hash-ref times 'unsamplable '())) 1000)))

(define (make-operation-table test-id)
  (for/list ([fn (in-list function-table)]
             #:when (or (not test-id) (equal? test-id (~a (object-name (first fn))))))
    (match-define (list ival-fn bf-fn itypes otype) fn)
    (define-values (iv256 bf256) (time-operation ival-fn bf-fn itypes otype))
    (define-values (iv4k bf4k)
      (if (set-member? slow-tests ival-fn)
          (values 1000000 1)
          (parameterize ([bf-precision 4096])
            (time-operation ival-fn bf-fn itypes otype))))
    (printf "~a ~aµs (~a×)\t~aµs (~a×)\n"
            (~a (object-name ival-fn) #:align 'left #:min-width 20)
            (~r iv256 #:precision '(= 3) #:min-width 8)
            (~r (/ iv256 bf256) #:precision '(= 2) #:min-width 4)
            (~r iv4k #:precision '(= 3) #:min-width 8)
            (~r (/ iv4k bf4k) #:precision '(= 2) #:min-width 4))
    
    (list (object-name ival-fn) iv256 (/ iv256 bf256) iv4k (/ iv4k bf4k))))

(define (timeline-push! timeline key args*)
  (match key
    ['outcomes
     (match-define (list status iter time*) args*)
     (define outcomes-hash (hash-ref timeline key))
     (match-define (list time num-points)
       (hash-ref outcomes-hash (list status iter) (λ () (list 0 0))))
     (hash-set! outcomes-hash (list status iter) (list (+ time time*) (+ num-points 1)))]
    [(or 'mixsample-rival-valid
         'mixsample-rival-all
         'mixsample-baseline-valid
         'mixsample-baseline-all)
     (define mixsample-hash (hash-ref timeline key))
     (match-define (list time* name precision) args*)
     (define time (hash-ref mixsample-hash (list name precision) (λ () 0)))
     (hash-set! mixsample-hash (list name precision) (+ time time*))]
    [else
     (error "Unknown key for timeline!")]))

(define (timeline->jsexpr timeline)
  (hash 'outcomes
        (for/list ([(key value) (in-hash (hash-ref timeline 'outcomes))])
          (list (first value) (second key) (first key) (second value)))
        
        'mixsample-rival-valid
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-rival-valid))])
          (list value (car key) (second key)))
        
        'mixsample-rival-all
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-rival-all))])
          (list value (car key) (second key)))
        
        'mixsample-baseline-valid
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-baseline-valid))])
          (list value (car key) (second key)))
        
        'mixsample-baseline-all
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-baseline-all))])
          (list value (car key) (second key)))))

(define (make-expression-table points test-id timeline-port)
  (newline)
  (define total-c 0.0)
  (define total-v 0.0)
  (define count-v 0.0)
  (define total-i 0.0)
  (define count-i 0.0)
  (define total-u 0.0)
  (define count-u 0.0)

  (define timeline (make-hash ; this hash is to be used for the plots
                    (list
                     (cons 'outcomes (make-hash))
                     (cons 'mixsample-rival-valid (make-hash))
                     (cons 'mixsample-baseline-valid (make-hash))
                     (cons 'mixsample-rival-all (make-hash))
                     (cons 'mixsample-baseline-all (make-hash)))))
  
  (define table
    (for/list ([rec (in-port read-json points)] 
               [i (in-naturals)]
               #:break (and test-id (> i (string->number test-id)))
               #:unless (and test-id (not (equal? (~a i) test-id))))
      (when test-id
        (pretty-print (map read-from-string (hash-ref rec 'exprs))))
      
      (match-define (list c-time v-num v-time i-num i-time u-num u-time)
          (time-exprs (time-expr rec timeline)))
      (set! total-c (+ total-c c-time))
      (set! total-v (+ total-v v-time))
      (set! count-v (+ count-v v-num))
      (set! total-i (+ total-i i-time))
      (set! count-i (+ count-i i-num))
      (set! total-u (+ total-u u-time))
      (set! count-u (+ count-u u-num))
      (define t-time (+ c-time v-time i-time u-time))
      (printf "~a: ~as ~as ~as ~as\n"
              (~a i #:align 'left #:min-width 3)
              (~r t-time #:precision '(= 3) #:min-width 8)
              (~r v-time #:precision '(= 3) #:min-width 8)
              (~r i-time #:precision '(= 3) #:min-width 8)
              (~r u-time #:precision '(= 3) #:min-width 8))
      (list i t-time c-time v-num v-time i-num i-time u-num u-time)))
  
  (when timeline-port
    (write-json (timeline->jsexpr timeline) timeline-port)
    (close-output-port timeline-port))
  
  (define total-t (+ total-c total-v total-i total-u))
  (printf "\nTotal Time: ~as\n" (~r total-t #:precision '(= 3)))
  (define footer
    (list "Total" total-t total-c count-v total-v count-i total-i count-u total-u))
  (values table footer))

(define (html-write port)
  (define sortable-css "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.css")
  (define sortable-js "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.js")
  (when port
    (fprintf port "<!doctype html><meta charset=utf-8 />")
    (fprintf port "<link href='~a' rel='stylesheet' />" sortable-css)
    (fprintf port "<script src='profile.js' defer></script>")
    (fprintf port "<script src='~a' async defer></script>" sortable-js)
    (fprintf port "<style>body { max-width: 100ex; margin: 3em auto; } td:nth-child(1n+2) { text-align: right; }</style>")))

(define current-heading #f)

(define (html-write-table port name cols)
  (set! current-heading cols)
  (when port
    (fprintf port "<h1>~a</h1>" name)
    (fprintf port "<table class=sortable>")
    (fprintf port "<thead><tr>")
    (for ([col (in-list cols)])
      (define name (match col [(list name _) name] [name name]))
      (fprintf port "<th>~a</th>" name))
    (fprintf port "</tr></thead><tbody>")))

(define (html-write-row port row)
  (when port
    (fprintf port "<tr>")
    (for ([cell (in-list row)] [heading (in-list current-heading)])
      (define unit (match heading [(list _ s) s] [_ ""]))
      (cond
        [(and (number? cell) (zero? cell))
         (fprintf port "<td></td>")]
        [(integer? cell)
         (fprintf port "<td>~a~a</td>" (~r cell #:group-sep " ") unit)]
        [(real? cell)
         (fprintf port "<td data-sort=~a>~a~a</td>" cell (~r cell #:precision '(= 2)) unit)]
        [else
         (fprintf port "<td><code>~a</code></td>" cell)]))
    (fprintf port "</tr>")))

(define (html-end-table port)
  (when port
    (fprintf port "</table>")))

(define (html-write-footer port row)
  (when port
    (fprintf port "<tfoot>")
    (html-write-row port row)))

(define (html-write-profile port)
  (when port
    (fprintf port "<section id='profile'><h1>Profiling</h1>")
    (fprintf port "<p class='load-text'>Loading profile data...</p></section>")))

(define (run test-id p timeline-port)
  (define operation-table
    (and
     (or (not test-id) (not (string->number test-id)))
     (make-operation-table test-id)))
  (define-values (expression-table expression-footer)
    (if (and p (or (not test-id) (string->number test-id)))
        (make-expression-table p test-id timeline-port)
        (values #f #f)))
  (list operation-table expression-table expression-footer))

(define (generate-ratio-plot dir)
  (define-values (sp out in err)
    (subprocess #f #f #f (find-executable-path "python3")
                "infra/ratio_plot.py"
                "-t" (format "~a/timeline.json" dir)
                "-o" (format "~a/ratio_plot.png" dir)))
  (printf "~a" (port->string err))
  (printf "~a" (port->string out)) ; macros for latex
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (subprocess-wait sp))

(define (generate-point-graph dir)
  (define-values (sp out in err)
    (subprocess #f #f #f (find-executable-path "python3")
                "infra/point_graph.py"
                "-t" (format "~a/timeline.json" dir)
                "-o" (format "~a/point_graph.png" dir)))
  (printf "~a" (port->string err))
  (printf "~a" (port->string out)) ; macros for latex
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (subprocess-wait sp))

(define (generate-histograms dir)
  (define-values (sp out in err)
    (subprocess #f #f #f (find-executable-path "python3")
                "infra/histograms.py"
                "-t" (format "~a/timeline.json" dir)
                "-o1" (format "~a/histogram_valid.png" dir)
                "-o2" (format "~a/histogram_all.png" dir)))
  (printf "~a" (port->string err))
  (printf "~a" (port->string out)) ; macros for latex
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (subprocess-wait sp))

(define (html-add-plot port path)
  (when port
    (fprintf port (format "<img src=\"~a\" width=\"320\" height=\"280\">" path))))

(define (html-add-histogram port path)
  (when port
    (fprintf port (format "<img src=\"~a\" width=\"650\" height=\"250\">" path))))

(define (generate-html html-port profile-port operation-table expression-table expression-footer dir)
  (html-write html-port)

  (when operation-table
    (define cols
      '("Operation" ("Time, 256b" "µs")  ("Slowdown" "×") ("Time, 4kb" "µs") ("Slowdown" "×")))
    (html-write-table html-port "Operation timing" cols)
    (for ([row (in-list operation-table)])
      (html-write-row html-port row))
    (html-end-table html-port))

  (when expression-table
    (define cols
      '("#" ("Total" "s") ("Compile" "s")
            "Valid" ("(s)" "s") "Invalid" ("(s)" "s") "Unable" ("(s)" "s")))
    (html-write-table html-port "Expression timing" cols)
    (for ([row (in-list expression-table)])
      (html-write-row html-port row))
    (when expression-footer
      (html-write-footer html-port expression-footer))
    (html-end-table html-port))

  (when expression-table
    (generate-ratio-plot dir)
    (generate-point-graph dir)
    (generate-histograms dir)
    (html-add-plot html-port "ratio_plot.png")
    (html-add-plot html-port "point_graph.png")
    (html-add-histogram html-port "histogram_valid.png")
    (html-add-histogram html-port "histogram_all.png"))

  (when profile-port
    (html-write-profile html-port)))

(define (profile-json-renderer profile-port)
  (lambda (p order)
    (when profile-port (write-json (profile->json p) profile-port))))

(module+ main
  (require racket/cmdline)
  (define dir #f)
  (define html-port #f)
  (define timeline-port #f)
  (define profile-port #f)
  (define n #f)
  (command-line
   #:once-each
   [("--dir") fn "Directory to produce html outputs"
              (set! dir fn)
              (when dir
                (set! timeline-port (open-output-file (format "~a/timeline.json" dir) #:mode 'text #:exists 'replace)))]
   [("--profile") fn "Produce a JSON profile"
                  (set! profile-port (open-output-file fn #:mode 'text #:exists 'replace))]
   [("--id") ns "Run a single test"
             (set! n ns)]
   #:args ([points "infra/points.json"])
   
   (match-define (list op-t ex-t ex-f)
     (if profile-port
         (profile #:order 'total #:delay 0.001 #:render (profile-json-renderer profile-port)
                  (run n (open-input-file points) timeline-port))
         (run n (open-input-file points) timeline-port)))
   (when dir
     (set! html-port (open-output-file (format "~a/index.html" dir) #:mode 'text #:exists 'replace))
     (generate-html html-port profile-port op-t ex-t ex-f dir))))


(define (point-bucketing
         timeline
         rival-status rival-time rival-exs
         baseline-status baseline-time baseline-exs
         sollya-status sollya-time sollya-exs
         rival-iter)

  (define (status-subbucketing status exs)
    (cond
      [(or (equal? exs (fl 0.0)) (equal? exs (fl -0.0))) (format "~a-zero" status)]
      [(flinfinite? exs) (format "~a-inf" status)]
      [else (format "~a-real" status)]))
  
  (cond
    ; Rival has produced valid outcomes
    [(equal? rival-status 'valid)
     (cond
       ; Every tool have succeded
       [(and (equal? 'valid sollya-status) (equal? 'valid baseline-status) (equal? rival-status 'valid)
             (< sollya-time (*sampling-timeout*)) (< baseline-time (*sampling-timeout*)) (< rival-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list "valid-sollya" rival-iter sollya-time))
        (timeline-push! timeline 'outcomes (list "valid-baseline" rival-iter baseline-time))
        (timeline-push! timeline 'outcomes (list "valid-rival" rival-iter rival-time))
        (if (fl= rival-exs sollya-exs)
            (timeline-push! timeline 'outcomes (list "sollya-correct-rounding" 0 0))
            (timeline-push! timeline 'outcomes (list "sollya-faithful-rounding" 0 0)))]

       ; Baseline and Rival have succeeded
       [(and (equal? 'valid baseline-status) (equal? rival-status 'valid)
             (< baseline-time (*sampling-timeout*)) (< rival-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-rival+baseline" rival-exs)
                                            rival-iter rival-time))]

       ; Baseline and Sollya have succeeded
       [(and (equal? 'valid sollya-status) (equal? 'valid baseline-status)
             (< sollya-time (*sampling-timeout*)) (< baseline-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-sollya+baseline" baseline-exs)
                                            rival-iter sollya-time))]

       ; Sollya and Rival have succeeded
       [(and (equal? 'valid sollya-status) (equal? rival-status 'valid)
             (< sollya-time (*sampling-timeout*)) (< rival-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-rival+sollya" rival-exs)
                                            rival-iter rival-time))]

       ; Only Rival has succeeded
       [(and (equal? rival-status 'valid) (< rival-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-rival-only" rival-exs)
                                            rival-iter rival-time))]
       
       ; Only Sollya has succeeded
       [(and (equal? 'valid sollya-status) (< sollya-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-sollya-only" sollya-exs)
                                            rival-iter sollya-time))]

       ; Only Baseline has succeeded
       [(and (equal? 'valid baseline-status) (< baseline-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-baseline-only" baseline-exs)
                                            rival-iter baseline-time))]
       
       ; timeout at all the tools
       [else
        (timeline-push! timeline 'outcomes (list "exit-baseline" rival-iter baseline-time))
        (timeline-push! timeline 'outcomes (list "exit-sollya" rival-iter sollya-time))
        (timeline-push! timeline 'outcomes (list "exit-rival" rival-iter rival-time))])]

    ; Rival has exited
    [(equal? rival-status 'unsamplable)
     (cond
       ; Sollya and Baseline have succeeded
       [(and (equal? 'valid sollya-status) (equal? 'valid baseline-status)
             (< sollya-time (*sampling-timeout*)) (< baseline-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-sollya+baseline" baseline-exs)
                                            rival-iter sollya-time))]

       ; Only Sollya has succeeded
       [(and (equal? 'valid sollya-status) (< sollya-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-sollya-only" sollya-exs)
                                            rival-iter sollya-time))]

       ; Only Baseline has succeeded
       [(and (equal? 'valid baseline-status) (< baseline-time (*sampling-timeout*)))
        (timeline-push! timeline 'outcomes (list
                                            (status-subbucketing "valid-baseline-only" baseline-exs)
                                            rival-iter baseline-time))]
       
       ; Points that every tools fail to evaluate when the precision is unreacheble
       [else
        (timeline-push! timeline 'outcomes (list "exit-baseline" rival-iter baseline-time))
        (timeline-push! timeline 'outcomes (list "exit-sollya" rival-iter sollya-time))
        (timeline-push! timeline 'outcomes (list "exit-rival" rival-iter rival-time))])]))
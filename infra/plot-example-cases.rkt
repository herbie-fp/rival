#lang racket

(require plot/no-gui pict)

(provide draw-sampled-chart draw-bad-result-chart)

(define global-plot-scale 4)
(define standard-width 500)
(define numbers-size 6)
(define labels-size 8)

(define (parameterize-plot-size width scale-width title bottom-axis left-axis output-file func)
  (define actual-w
    (inexact->exact (floor (* width global-plot-scale))))
  (define f-size (inexact->exact (floor (* global-plot-scale numbers-size (/ width 400) (/ 1.0 scale-width)))))
  (define graph-pict
    (parameterize ([plot-width actual-w]
                   [plot-height (inexact->exact
                                 (floor (* 300 global-plot-scale)))]
                   [plot-x-label #f]
                   [plot-y-label #f]
                   [plot-font-size
                    f-size])
      (func)))
  (define font-pixel-height (inexact->exact (floor (* global-plot-scale labels-size (/ width 400) (/ 1.0 scale-width)))))
  (define title-pict
    (if (equal? title "")
        (filled-rectangle 0 0)
        (text title "Computer Modern"
              font-pixel-height)))
  (define left-pict
    (if (equal? left-axis "")
        (filled-rectangle 0 0)
        (text left-axis "Computer Modern"
              font-pixel-height (/ pi 2))))
  (define bottom-pict
    (if (equal? bottom-axis "")
        (filled-rectangle 0 0)
        (text bottom-axis "Computer Modern"
              font-pixel-height)))
  (define space-block (colorize (filled-rectangle (pict-height title-pict)
                                                  (pict-height title-pict)) "white"))

  (define without-background
    (hc-append left-pict space-block
               (vc-append title-pict
                          space-block
                          (vc-append
                           graph-pict
                           space-block
                           bottom-pict))))

  (define res
    (lt-superimpose
     (colorize (filled-rectangle (pict-width without-background) (pict-height without-background)) "white")
     without-background))

  (send (pict->bitmap res) save-file output-file 'png))


(define (draw-sampled-chart mathematica-sampled mathematica-error rival-sampled rival-error output)
  (parameterize-plot-size
   300
   0.5
   "Resolved Points"
   ""
   "Number of Points"
   output
   (lambda ()
     (plot-pict
      (list (stacked-histogram (list
                                (vector 'rival (list rival-sampled rival-error))
                                (vector 'mathematica (list mathematica-sampled mathematica-error)))
                               #:colors `("Green" "Orange")
                               #:line-colors `("Green" "Orange")))))))


(define (draw-bad-result-chart mathematica-list rival-list output)
  (parameterize-plot-size
   300
   0.5
   "Failure to Sample Results"
   ""
   "Number of Points"
   output
   (lambda ()
     (plot-pict
      (list (stacked-histogram (list
                                (vector 'rival rival-list)
                                (vector 'mathematica mathematica-list))
                               #:labels `("Unsamplable" "Unknown" "Out-of-memory")
                               #:colors `("Blue" "Orange" "Red")
                               #:line-colors `("Blue" "Orange" "Red")))))))


#lang racket

(require plot/no-gui pict)

(provide draw-chart)

;; global scale changes the relative size of the axis
(define global-plot-scale 0.5)
(define standard-width 500)
(define numbers-size 6)
(define labels-size 8)

(define (parameterize-plot-size width scale-width title bottom-axis left-axis output-file func)
  (define actual-w
    (inexact->exact (floor (* width global-plot-scale))))
  ;; increase font size so that when it is scaled down into the latex pdf, it is the correct size
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

(define (draw-chart mathematica-list rival-list output)
  (define colors `((3 97 17) "Green" "Orange" "Red" "Black" "Yellow"))
  (parameterize ()
                (parameterize-plot-size
                 1500
                 1
                 ""
                 ""
                 ""
                 output
                 (lambda ()
                   (plot-pict
                    (stacked-histogram (list
                                        (vector 'rival rival-list)
                                        (vector 'math mathematica-list))
                                       #:invert? #t
                                       #:colors colors
                                       #:line-colors colors))))))
  

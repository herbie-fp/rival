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

(define (make-interval start-x end-x interval-color interval-height interval-thickness y-interval-pos)
  (list
   (hrule (+ y-interval-pos (/ interval-height 2)) start-x end-x
                            #:width interval-thickness #:color interval-color)
   (vrule start-x y-interval-pos (+ y-interval-pos interval-height) #:width interval-thickness #:color interval-color)
   (vrule end-x y-interval-pos (+ y-interval-pos interval-height)
          #:width interval-thickness #:color interval-color)))

(define (draw-chart points-search-saves-domain points-search-saves-unsamplable mathematica-list rival-list output)
  (println mathematica-list)
  (println rival-list)
  (define colors `((3 97 17) (127 227 116) "Orange" "Red" "Black" "Yellow"))
  (define interval-color `(38 107 255))
  (define interval-height 0.3)
  (define interval-thickness 3)
  (define y-interval-pos 2)
  (define unsamplable-pos (+ (first rival-list) (second rival-list)))
  (parameterize ([plot-y-ticks no-ticks]
                 [plot-x-far-ticks no-ticks]
                 [plot-x-far-axis? #f]
                 [plot-y-axis? #f])
                (parameterize-plot-size
                 1500
                 1
                 ""
                 ""
                 ""
                 output
                 (lambda ()
                   (plot-pict
                    #:y-max (+ y-interval-pos interval-height)
                    (list
                     (make-interval (first rival-list) (+ (first rival-list) points-search-saves-domain)
                                    interval-color interval-height interval-thickness y-interval-pos)
                     (make-interval unsamplable-pos (+ unsamplable-pos points-search-saves-unsamplable)
                                    interval-color interval-height interval-thickness y-interval-pos)
                     (stacked-histogram (list
                                        (vector 'math mathematica-list)
                                        (vector 'rival rival-list))
                                       #:invert? #t
                                       #:colors colors
                                       #:line-colors colors)))))))
  

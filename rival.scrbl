#lang scribble/manual

@(require (for-label "main.rkt" racket/base math/bigfloat))
@(require scribble/example racket/sandbox racket/pretty)
@(define example-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket #:requires '(rival math/bigfloat racket/pretty))))
@(call-in-sandbox-context example-eval
             (lambda ()
               (current-print (dynamic-require 'racket/pretty 'pretty-print-handler))))

@title{Rival: Real Computation via Interval Arithmetic}
@author{Pavel Panchekha}
@defmodule[rival]

Rival is an advanced interval arithmetic library for
arbitrary-precision computation of complex mathematical expressions.
Its interval arithmetic is valid and attempts to be tight.
Besides the standard intervals, Rival also supports boolean intervals,
error intervals, and movability flags, as described in
@hyperlink["https://arxiv.org/abs/2107.05784"]{"An Interval Arithmetic
for Robust Error Estimation"}.

Rival is a part of the @hyperlink["https://herbie.uwplse.org"]{Herbie project},
and is developed @hyperlink["https://github.com/herbie-fp/rival"]{on Github}.

Rival can be to evaluate real-number expressions:

@examples[#:eval example-eval #:label #f
(define expr '(- (sin x) (- x (/ (pow x 3) 6))))
(define machine (rival-compile (list expr) '(x) (list flonum-discretization)))
(rival-apply machine (vector (bf 0.5)))
(rival-apply machine (vector (bf 0.25)))
]

It can also be used as an interval-arithmetic library:

@examples[#:eval example-eval #:label #f
(bf-precision 20)
(define x (ival 2.bf 3.bf))
x
(ival-add x x)
(ival-sqrt x)
]

Rival is fast, accurate, and sound. We believe it to be a
state-of-the-art implementation, competitive with Sollya/MPFI,
Calcium/Arb, and Mathematica.

@include-section["eval.scrbl"]
@include-section["core.scrbl"]
@include-section["profile.scrbl"]

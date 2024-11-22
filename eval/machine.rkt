#lang racket/base

(provide (struct-out discretization)
         (struct-out rival-machine)
         *rival-max-precision*
         *rival-min-precision*
         *rival-profile-executions*
         *ampl-tuning-bits*
         *last-iteration*
         *sampling-iteration*
         *lower-bound-early-stopping*
         *base-tuning-precision*)

(define *rival-max-precision* (make-parameter 10000))
(define *rival-min-precision* (make-parameter 20))
(define *rival-profile-executions* (make-parameter 1000))
(define *lower-bound-early-stopping* (make-parameter #t))

(struct discretization (target convert distance))

(struct rival-machine
        (arguments instructions
                   outputs
                   discs
                   registers
                   repeats
                   precisions
                   incremental-precisions
                   output-distance
                   initial-precision
                   [iteration #:mutable]
                   [bumps #:mutable]
                   [profile-ptr #:mutable]
                   profile-instruction
                   profile-number
                   profile-time
                   profile-precision))

(define *ampl-tuning-bits* (make-parameter 5))
(define *sampling-iteration* (make-parameter 0))
(define *last-iteration* (make-parameter #f))
(define *base-tuning-precision* (make-parameter 10))

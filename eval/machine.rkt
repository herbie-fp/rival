#lang racket/base

(provide (struct-out discretization)
         (struct-out rival-machine)
         *rival-max-precision*
         *rival-min-precision*
         *rival-max-iterations*
         *rival-profile-executions*
         *ampl-tuning-bits*
         *sampling-iteration*
         *base-tuning-precision*)

(define *rival-max-precision* (make-parameter 10000))
(define *rival-min-precision* (make-parameter 20))
(define *rival-max-iterations* (make-parameter 5))
(define *rival-profile-executions* (make-parameter 1000))
(define *early-stopping-undershoot* (make-parameter 0))

(struct discretization (target convert distance))

(struct rival-machine
        (arguments instructions
                   outputs
                   discs
                   registers
                   repeats
                   precisions
                   initial-precisions
                   output-distance
                   [iteration #:mutable]
                   [bumps #:mutable]
                   [profile-ptr #:mutable]
                   profile-instruction
                   profile-number
                   profile-time
                   profile-precision))

(define *ampl-tuning-bits* (make-parameter 5))
(define *sampling-iteration* (make-parameter 0))
(define *base-tuning-precision* (make-parameter 10))

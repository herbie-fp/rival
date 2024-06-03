#lang racket/base

(require (only-in math/private/bigfloat/mpfr bf-precision) racket/match racket/function)
(require "machine.rkt" "adjust.rkt")
(provide rival-machine-load rival-machine-run rival-machine-return rival-machine-adjust)

(define (rival-machine-load machine args)
  (vector-copy! (rival-machine-registers machine) 0 args)
  #;(set-rival-machine-iteration! machine 0))

(define (rival-machine-record machine name number precision time)
  (define profile-ptr (rival-machine-profile-ptr machine))
  (define profile-instruction (rival-machine-profile-instruction machine))
  (when (< profile-ptr (vector-length profile-instruction))
    (define profile-number (rival-machine-profile-number machine))
    (define profile-time (rival-machine-profile-time machine))
    (define profile-precision (rival-machine-profile-precision machine))
    (vector-set! profile-instruction profile-ptr name)
    (vector-set! profile-number profile-ptr number)
    (vector-set! profile-precision profile-ptr precision)
    (vector-set! profile-time profile-ptr time)
    (set-rival-machine-profile-ptr! machine (add1 profile-ptr))))

(define (rival-machine-run machine)
  (define ivec (rival-machine-instructions machine))
  (define varc (vector-length (rival-machine-arguments machine)))
  (define precisions
    (if (zero? (rival-machine-iteration machine))
        (rival-machine-initial-precisions machine)
        (rival-machine-precisions machine)))
  (define repeats (rival-machine-repeats machine))
  (define vregs (rival-machine-registers machine))

  ; parameter for sampling histogram table
  (define first-iter? (zero? (rival-machine-iteration machine)))

  (for ([instr (in-vector ivec)]
        [n (in-naturals varc)]
        [precision (in-vector precisions)]
        [repeat (in-vector repeats)]
        #:unless (and (not first-iter?) repeat))
    (define start (current-inexact-milliseconds))
    (parameterize ([bf-precision precision])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (define name (object-name (car instr)))
    (define time (- (current-inexact-milliseconds) start))
    (rival-machine-record machine name n precision time)))

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a)
     (op (vector-ref regs a))]
    [(list op a b)
     (op (vector-ref regs a)
         (vector-ref regs b))]
    [(list op a b c)
     (op (vector-ref regs a)
         (vector-ref regs b)
         (vector-ref regs c))]
    [(list op args ...)
     (apply op (map (curryr vector-ref regs) args))]))

(define (rival-machine-return machine)
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define rootvec (rival-machine-outputs machine))
  #;(set-rival-machine-iteration! machine (add1 (rival-machine-iteration machine)))
  (for/vector #:length (vector-length rootvec)
              ([root (in-vector rootvec)] [disc (in-vector discs)])
    (vector-ref vregs root)))

(define (rival-machine-adjust machine)
  (define iter (rival-machine-iteration machine))
  (unless (zero? iter)
    (define start (current-inexact-milliseconds))
    (backward-pass machine)
    (rival-machine-record machine 'adjust 0 (* iter 1000) (- (current-inexact-milliseconds) start))))

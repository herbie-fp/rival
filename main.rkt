#lang racket/base

(require "ops.rkt" "eval/main.rkt")
(provide (all-from-out "ops.rkt" "eval/main.rkt"))

(module+ main
  (require "eval/repl.rkt")
  (rival-repl))

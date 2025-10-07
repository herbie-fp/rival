
# Organization

- The MPFR bindings live in `mpfr.rkt`; the base interval definition
  is in `ops/core.rkt`; the interval arithmetic functions are in
  `ops/`; the real evaluator is in `eval/`; the REPL is in `repl.rkt`.
  The tests are in `test.rkt`; the nightly is `time.rkt`. The nightly
  also has various helper functions in `infra/`
- When editing an interval arithmetic function, always run the tests.
  Run tests with `racket -y test.rkt add` (for the right function).

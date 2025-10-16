
# Organization

- The MPFR bindings live in `mpfr.rkt`; the base interval definition
  is in `ops/core.rkt`; the interval arithmetic functions are in
  `ops/`; the real evaluator is in `eval/`; the REPL is in `repl.rkt`.
  The tests are in `test.rkt`; the nightly is `time.rkt`. The nightly
  also has various helper functions in `infra/`
- Mandatory auto-format. Always run `make fmt` before finishing your
  task to format your code correctly.
- When editing an interval arithmetic function, always run the tests.
  Run tests with `racket -y test.rkt add` (for the right function).
- When editing the eval stuff, always run the tests. Run them with
  `racket -y time.rkt --id N ../../infra/points.json`. The number `N`
  is a benchmark ID; some common choices are 12 and 57, but there are
  a bit over 500 in total and sometimes you're asked to focus on a
  particular one. Smaller numbers are faster to run.

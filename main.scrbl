#lang scribble/manual
@(require (for-label "main.rkt" racket/base math/bigfloat))
@(require scribble/example racket/sandbox)
@(define example-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket #:requires '(rival math/bigfloat))))

@title{Rival: An Interval Arithmetic for Real Computation}
@author{Pavel Panchekha}

@defmodule[rival]

@section{Introduction}

Rival is an advanced interval arithmetic library for
arbitrary-precision computation of complex mathematical expressions.
Its interval arithmetic is sound and attempts to be weakly complete.
Besides the standard intervals, Rival also supports boolean intervals,
error intervals, and movability flags, as described in
@hyperlink["https://arxiv.org/abs/2107.05784"]{"An Interval Arithmetic
for Robust Error Estimation"}.

@section{Intervals}

A standard Rival interval contains two @racket[bigfloat?] values and
include both endpoints. Neither endpoint is allowed to be @racket[+nan.bf].

@defproc[(ival [lo (or/c bigfloat? boolean?)]
             [hi (or/c bigfloat? boolean?) lo]) ival?]{
Constructs an interval given two endpoints @racket[lo] and
@racket[hi]. If @racket[hi] isn't given, the interval contains a
single point, @racket[lo]. Single-point intervals are considered immovable,
while all other intervals are considered movable. If either endpoint
is @racket[+nan.bf], a guaranteed error interval is returned.

You can also use the @racket[ival] form for matches, as in:

@examples[#:eval example-eval
(match-define (ival lo hi) (ival-sqrt (ival 0.bf 2.bf)))
]

This is the preferred way to access interval endpoints.
}

@defproc[(ival? [obj any/c]) boolean?]{
  Determines whether an object is an interval.
}

@defproc*[([(ival-lo [x ival?]) (or/c bigfloat? boolean?)]
          [(ival-hi [x ival?]) (or/c bigfloat? boolean?)])]{
These accessor methods provide an alternative way of accessing the two
endpoints of an interval when a @racket[match] is not convenient.
}

@defproc*[([(ival-pi) ival?]
           [(ival-e) ival?])]{
Rival also provides two interval constants. Note that, since π and e
are not exact @racket[bigfloat?] values, these constants are
implemented a functions, which return different intervals at different
@racket[bf-precision]s.
}

@section{Interval Operations}

Rival provides a large set of interval operations. All of these
operations are sound, meaning that the output interval always contains
all valid outputs from points in the input intervals.

Most operations are also weakly complete, meaning that the endpoints
of the output interval come from some point in the input intervals
(rounded outwards). Not all operations are weakly complete, however.

These operations have their output precision determined by
@racket[bf-precision].

@deftogether[(
  @defproc[(ival-add [a ival?] [b ival?]) ival?]
  @defproc[(ival-sub [a ival?] [b ival?]) ival?]
  @defproc[(ival-neg [a ival?]) ival?]
  @defproc[(ival-mul [a ival?] [b ival?]) ival?]
  @defproc[(ival-div [a ival?] [b ival?]) ival?]
  @defproc[(ival-fma [a ival?] [b ival?] [c ival?]) ival?]
  @defproc[(ival-fabs [a ival?]) ival?]
  @defproc[(ival-sqrt [a ival?]) ival?]
  @defproc[(ival-cbrt [a ival?]) ival?]
  @defproc[(ival-hypot [a ival?] [b ival?]) ival?]
  @defproc[(ival-exp [a ival?]) ival?]
  @defproc[(ival-exp2 [a ival?]) ival?]
  @defproc[(ival-exp2m1 [a ival?]) ival?]
  @defproc[(ival-log [a ival?]) ival?]
  @defproc[(ival-log2 [a ival?]) ival?]
  @defproc[(ival-log10 [a ival?]) ival?]
  @defproc[(ival-log1p [a ival?]) ival?]
  @defproc[(ival-log1b [a ival?]) ival?]
  @defproc[(ival-pow [a ival?] [b ival?]) ival?]
  @defproc[(ival-sin [a ival?]) ival?]
  @defproc[(ival-cos [a ival?]) ival?]
  @defproc[(ival-tan [a ival?]) ival?]
  @defproc[(ival-asin [a ival?]) ival?]
  @defproc[(ival-acos [a ival?]) ival?]
  @defproc[(ival-atan [a ival?]) ival?]
  @defproc[(ival-atan2 [a ival?] [b ival?]) ival?]
  @defproc[(ival-sinh [a ival?]) ival?]
  @defproc[(ival-cosh [a ival?]) ival?]
  @defproc[(ival-tanh [a ival?]) ival?]
  @defproc[(ival-asinh [a ival?]) ival?]
  @defproc[(ival-acosh [a ival?]) ival?]
  @defproc[(ival-atanh [a ival?]) ival?]
  @defproc[(ival-erf [a ival?]) ival?]
  @defproc[(ival-erfc [a ival?]) ival?]
  @defproc[(ival-fmod [a ival?] [b ival?]) ival?]
  @defproc[(ival-remainder [a ival?] [b ival?]) ival?]
  @defproc[(ival-rint [a ival?]) ival?]
  @defproc[(ival-round [a ival?]) ival?]
  @defproc[(ival-ceil [a ival?]) ival?]
  @defproc[(ival-floor [a ival?]) ival?]
  @defproc[(ival-trunc [a ival?]) ival?]
  @defproc[(ival-fmin [a ival?] [b ival?]) ival?]
  @defproc[(ival-fmax [a ival?] [b ival?]) ival?]
  @defproc[(ival-copysign [a ival?] [b ival?]) ival?]
  @defproc[(ival-fdim [a ival?] [b ival?]) ival?]
)]{
  These are all interval functions with arguments in the order
  corresponding to the same-name @code{math.h} functions. Barring
  bugs, all are sound. Most are weakly complete, though some more
  complex functions aren't, including @racket[ival-pow],
  @racket[ival-fma], @racket[ival-fmod], and @racket[ival-atan2]. Even
  these fuctions still make a best-effort attempt to produce
  relatively narrow intervals. For example, @racket[ival-fma] uses
  the formula @code{(fma a b c) = (+ (* a b) c)}, meaning that it
  accumulates multiple rounding errors leading the resulting interval
  to not be maximally tight, but typically still pretty close.
}

@defproc[(ival-sort [lst (listof ival?)]
                  [< (-> (or/c bigfloat? boolean?)
                        (or/c bigfloat? boolean?)
                        boolean?)])
        (listof ival?)]{
  Sorts a list of intervals using a comparator function.
}

Additionally, Rival provides simple helper methods to define your own
interval functions.

@deftogether[(
  @defproc[(monotonic->ival [fn (-> bigfloat? (or/c bigfloat? boolean?))])
          (-> ival? ival?)]
  @defproc[(comonotonic->ival [fn (-> bigfloat? (or/c bigfloat? boolean?))])
          (-> ival? ival?)]
)]{
  These functions lift a (weakly) (co-)monotonic bigfloat function to
  a function on intervals. A weakly monotonic function is one where
  larger inputs produce larger (or equal) outputs; a weakly
  co-monotonic function is one where larger inputs produce smaller (or
  equal) outputs. For example:

  @examples[#:eval example-eval
    (define ival-cube (monotonic->ival (lambda (x) (bf* x x x))))
    (ival-cube (ival -1.bf 3.bf))
  ]
  
  Note that if a non-(co-)monotonic function is passed, the results
  will not be sound.
}

@defproc[(ival-union [a ival?] [b ival?]) ival?]{
  Computes the union of two intervals. Maintains error flags, and
  movability flags when possible.
}

@defproc[(ival-split [a ival?] [x bigfloat?]) (values (or/c ival? #f) (or/c ival? #f))]{
  Splits an interval at a point, returning the two halves of that
  interval on either side of the split point. If the point is not
  within the interval, one of the two output intervals will be
  @racket[#f] instead. This can be used to define simple,
  non-monotonic interval functions. For example:

  @examples[#:eval example-eval
  (define (ival-fabs x)
    (match/values (ival-split x 0.bf)
      [(#f hi) hi]
      [(lo #f) (ival-neg lo)]
      [(lo hi) (ival-union (ival-neg lo) hi)]))
  (ival-fabs (ival -1.bf 1.bf))
  ]
}

@section{Boolean Intervals}

Rival supports @emph{boolean} intervals, whose endpoints are
@racket[boolean?].

@margin-note{
Note that an @racket[ival?] must have either two @racket[bigfloat?]
intervals, or two @racket[boolean?] intervals, never one of each.
However, this constraint is not enforced by contracts or the type
system.
}

In a boolean interval, @racket[#f] is considered less than
@racket[#t], yielding three boolean interval values:

@deftogether[(
  @defthing[ival-true ival?]
  @defthing[ival-false ival?]
  @defthing[ival-uncertain ival?]
)]{
  Shorthands for @racket[(ival #f)] and @racket[(ival #t)],
  representing true and false, and @racket[(ival #f #t)], representing
  an uncertain value that might be true or false.
}

@deftogether[(
  @defproc[(ival-< [a ival?] ...) ival?]
  @defproc[(ival-<= [a ival?] ...) ival?]
  @defproc[(ival-> [a ival?] ...) ival?]
  @defproc[(ival->= [a ival?] ...) ival?]
  @defproc[(ival-== [a ival?] ...) ival?]
)]{
  Comparison operations, where all input intervals must be real
  intervals while the output interval is boolean. The answer can be
  @racket[ival-true] or @racket[ival-false] when, for example,
  intervals do not overlap. However, in many cases
  @racket[ival-uncertain] is the only possible answer:
  
  @examples[#:eval example-eval
  (ival-< (ival -1.bf 3.bf) (ival 1.bf 5.bf)) 
  ]

  With zero or one argument, these functions yield @racket[ival-true],
  while with more arguments they function as chained comparators much
  like the analogous Racket comparison functions like @racket[<].
  Their behavior with illegal intervals is undefined.
}

@defproc[(ival-!= [a ival?] ...) ival?]{
  Returns @racket[(ival #t)] only if all input intervals do not
  overlap. In this way it is like a @code{distinct?} method.
}

Boolean intervals can be combined logically.

@deftogether[(
  @defproc[(ival-and [a ival?] ...) ival?]
  @defproc[(ival-or [a ival?] ...) ival?]
  @defproc[(ival-not [a ival?]) ival?]
)]{
  The expected logical operations extended to boolean intervals using
  a standard tripartite logic.
}

@defproc[(ival-if [cond ival?] [if-true ival?] [if-false ival?]) ival?]{
  Here, @racket[cond] must be a boolean interval, while
  @racket[if-true] and @racket[if-false] should be intervals of the
  same type, either both real or both boolean. If @racket[cond] is
  uncertain, the union of @racket[if-true] and @racket[if-false] is
  returned.

  Note that typically, uses of @racket[ival-if] are incomplete because
  they are not flow-sensitive. For example, @racket[(if (< x 0) (- x)
  x)] is always non-negative, but:

  @examples[#:eval example-eval
  (define (bad-ival-fabs x)
    (ival-if (ival-< x (ival 0.bf)) (ival-neg x) x))
  (bad-ival-fabs (ival -1.bf 1.bf))
  ]

  The reason for this is that both branches, @racket[(ival-neg x)] and
  @racket[x], evaluate to @racket[(ival -1.bf 1.bf)]; the condition
  @racket[(ival-< x (ival 0.bf))] does not refine the value of
  @racket[x] in either branch.
}

@section{Error Intervals}

Sometimes an interval will contain invalid inputs to some function.
For example, @racket[sqrt] is undefined for negative inputs! In cases
like this, Rival's output interval will only consider valid inputs:

@examples[#:eval example-eval
(ival-sqrt (ival -1.bf 1.bf))
]

If none of the values in the interval are valid, a special
@racket[ival-illegal] value will be returned:

@examples[#:eval example-eval
(ival-sqrt (ival (bf -4) (bf -2)))
]

Moreover, when an interval is computed by discarding invalid inputs,
special error flags are set that can be retrieved with
@racket[ival-error?]:

@examples[#:eval example-eval
(ival-error? (ival-sqrt (ival 1.bf 4.bf)))
(ival-error? (ival-sqrt (ival -1.bf 1.bf)))
(ival-error? (ival-sqrt (ival (bf -4) (bf -2))))
]

@defproc[(ival-error? [a ival?]) ival?]{
  Returns a boolean interval indicating whether any inputs were
  discarded when computing @racket[a]. These flags are "sticky":
  further computations on @racket[a] will maintain the already-set
  error flags.
}

@defthing[ival-illegal ival?]{
  A real interval containing no values, with error flags indicating
  that all valid values have been discarded. All operations on
  @racket[ival-illegal] yield an illegal interval.
}

@section{Movability flags}

The typical use case for Rival is to recompute a certain expression at
ever higher @racket[bf-precision], until the computed interval is
narrow enough. However, interval arithmetic is not complete. For
example, due to the limitations of @racket[math/bigfloat]'s underlying
MPFR library, it's impossible to compute @racket[(/ (exp x) (exp x))]
for large enough values of @racket[x]:

@examples[#:eval example-eval
  (define x (ival (bf 1e100)))
  (ival-div (ival-exp x) (ival-exp x))
]

The same result will hold for any @racket[bf-precision]. While it's
impossible to detect this in all cases, Rival provides support for
@emph{movability flags} that can detect many such instances
automatically. Movability flags are correctly propagated by all of
Rival's supported libraries, and are set by a couple of functions such
as @racket[ival-exp].

The only access to movability flags is via @racket[close-enough->ival].

@defproc[(close-enough->ival [fn (-> (or/c bigfloat? boolean?)
                                 (or/c bigfloat? boolean?) boolean?)])
        (-> ival? ival?)]{
  The argument to @racket[close-enough] is a function that determines
  whether two interval endpoints are close enough. The returned
  function can be used to determine if an interval will ever be close
  enough. The result will be @racket[ival-true] if it is already close enough;
  @racket[ival-uncertain] in most cases; and @racket[ival-false] if
  Rival can prove that no evaluation at a higher precision can yield a
  close enough interval:
  
  @examples[#:eval example-eval
    (define (close-enough x y) (bf< (bf- y x) 1.bf))
    (define ival-close-enough? (close-enough->ival close-enough))
    (ival-close-enough? (ival 1.bf))
    (ival-close-enough? (ival 1.bf 5.bf))
    (define x (ival (bf 1e100)))
    (ival-close-enough? (ival-div (ival-exp x) (ival-exp x)))
  ]

  Using movability flags via @racket[close-enough->ival] can often cut
  short tedious timeout loops on impossible examples.
}

Note that, because of movability flags, there are actually many
functionally different intervals with the same endpoints. For example,
there are actually four different uncertain boolean intervals: regular
uncertain intervals, which might become certain at higher precision;
biased-true intervals, which are uncertain at this precision but which
can't be disproven true at any precision; biased-false intervals; and
the provably uncertain intervals, which can't be made certain at any
higher precision. Movability flags at a ton of complexity to interval
arithmetic, but Rival hides all of that from you inside
@racket[close-enough->ival].

Interval Arithmetic for Real Computation
========================================

Rival is an interval arithmetic library for Racket, intended for real
computation. Rival supports Racket 7.0+ and depends only on the
standard `math` library, including `math/bigfloat`.

Using Rival
-----------

Rival's main data structure is the `ival`, which represents the set of
extended real numbers between two points (inclusive both ends). Real
numbers, for Rival, are represented by bigfloats. For example:

    > (require rival math/bigfloat)
    > (ival 2.bf 3.bf)
    (ival 2 3)

You can also easily create single-point intervals with `mk-ival`:

    > (ival 2.bf 2.bf)
    (ival 2 2)
    > (mk-ival 2.bf)
    (ival 2 2)

Rival provides interval implementations of all of the standard
arithmetic functions:

    > (ival-mult (ival 2.bf 3.bf) (ival 1.bf 2.bf))
    (ival 2 6)
    > (ival-sin (ival 2.bf 3.bf))
    (ival 0.1411... 0.9092...)

Note intervals are always rounded outwards. Functions like `remainder`
and `pow` match their counterparts in C's `math.h`.

Control flow
------------

Rival supports _boolean intervals_, where the bounds of the intervals
are booleans instead of real numbers. This allows Rival to support
comparisons, boolean operations, and conditionals:

    > (ival-< (ival 1.bf 3.bf) (ival 2.bf 4.bf))
    (ival #f #t)
    > (ival-and (ival-< (ival 1.bf 3.bf) (ival 2.bf 4.bf))
                (ival-bool #f))
    (ival #f #f)
    > (if (ival #f #t)
          (ival 1.bf 2.bf)
          (ival 3.bf 4.bf))
    (ival 1.bf 4.bf)

In addition, intervals contain _error intervals_ tracking whether any
functions could/must be called outside their domain. These are
accessible with `ival-err?` and `ival-err`. For example, since square
root is defined only for positive values, calling it on `[-1, 1]`
could, but doesn't have to, raise an error:

    > (define i (ival-sqrt (ival -1.bf 1.bf)))
    > i
    (ival 0 1)
    > (ival-err? i)
    #t
    > (ival err i)
    #f

Meanwhile, calling it on `[-2, -1]` always produces an error:

    > (define i (ival-sqrt (ival -2.bf -1.bf)))
    > i
    (ival +nan.bf +nan.bf)
    > (ival-err? i)
    #t
    > (ival err i)
    #t

Movability
----------

Since Rival internally uses bigfloat computation, it is sensitive to
bigfloat rounding. Intervals may shrink (though they cannot grow) when
computed at a higher precision. However, in some cases it is known
this will not occur, largely due to overflow. In those cases, the
interval is marked fixed, or immovable:

    > (ival-exp (mk-ival (bf 1e100)))
    (ival ... +inf.bf)
    > (ival-hi-fixed? (ival-exp (mk-ival (bf 1e100))))
    #t

Movability flags are propagated

Rival's guarantees
------------------

Rival attempts to guarantee (and has random tests for) the following
properties:

 + If `x ∈ I`, `f(x) ∈ f(I)`,
   where both functions are computed at the same biginterval precision.
 + If `f(I) = [l, h]`, then `∃ x ∈ I, f(x) = l` (if `f` rounds down),
   and likewise for `h`.

Completeness may be violated in edge cases for complex functions such
as `fmod` and `pow`.

Error flags also aim to be sound and complete:

 + If `f(x)` raises an error and `x ∈ I`, then `err?(f(I))`.
 + If `err?(f(I))`, then `∃ x ∈ I` such that `f(x)` raises an error

However, Rival sometimes idiosyncratically interprets some error
conditions as valid. For example, `sin(+inf.bf)` is interpreted as the
_interval_ `[-1, 1]` instead of as an error condition; this impacts
both soundness and completeness.

Finally movability flags aim at soundness, though not yet completeness:

 + If `f(I).hi.fixed?` at a precision `p`,
   then `f(I).hi` is the same at all precisions `q > p`
   
Besides functional correctness, Rival guarantees termination for each
operation and error-freedom for correctly-invoked operations. (Its
contracts are not yet strong enough to prevent ill-typed invocations.)

Speed is fairly good, with attention paid to avoid unnecessary
bigfloat computations.

Please report any failures of any of the above properties as bugs.



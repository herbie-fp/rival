#lang racket

(require "core.rkt" "pow.rkt")

(provide
 ival? (rename-out [ival-expander ival] [ival-hi-val ival-hi] [ival-lo-val ival-lo])
 ival-union ival-split monotonic->ival comonotonic->ival
 ival-illegal ival-pi ival-e ival-bool
 ival-add ival-sub ival-neg ival-mult ival-div ival-fma       
 ival-fabs ival-sqrt ival-cbrt ival-hypot ival-exp ival-exp2 ival-expm1
 ival-log ival-log2 ival-log10 ival-log1p ival-logb ival-pow ival-pow2
 ival-sin ival-cos ival-tan
 ival-asin ival-acos ival-atan ival-atan2 ival-sinh ival-cosh ival-tanh
 ival-asinh ival-acosh ival-atanh ival-erf ival-erfc ival-lgamma ival-tgamma    
 ival-fmod ival-remainder ival-rint ival-round ival-ceil ival-floor ival-trunc     
 ival-fmin ival-fmax ival-copysign ival-fdim ival-sort      
 ival-< ival-<= ival-> ival->= ival-== ival-!= ival-if ival-and ival-or ival-not       
 ival-error? ival-assert ival-then close-enough->ival
 ;; Deprecated
 ival-lo-fixed? ival-hi-fixed? ival-err? ival-err mk-ival)
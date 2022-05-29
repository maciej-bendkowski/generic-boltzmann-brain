# Generic Boltzmann Brain [![License](https://img.shields.io/badge/license-BSD--3-orange.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
-------------------------

`generic-boltzmann-brain` is a template Haskell library which allows its users
to automatically generate efficient multi-parametric analytic *Boltzmann
samplers* for algebraic data types.

**Disclaimer**

Please bear in mind that the current project is still under active development
and should therefore be treated as a working prototype. Comments, critique, and
feature requests are most welcome.

### Citing Generic Boltzmann Brain
If you use *Generic Boltzmann Brain* or its components for published work,  we encourage you to cite the accompanying paper:

*Maciej Bendkowski*

[Automatic compile-time synthesis of entropy-optimal Boltzmann samplers](https://maciej-bendkowski.github.io/generic-boltzmann-brain.pdf)

### Quick overview

`generic-boltzmann-brain` constructs [Boltzmann
samplers](https://en.wikipedia.org/wiki/Boltzmann_sampler) generating random
inhabitants of user-declared algebraic data types. For instance, given the
following `BinTree` data type

```hs
import Data.Boltzmann

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)
```

one can construct a corresponding Boltzmann sampler in a single line of code

``` hs
mkDefBoltzmannSampler ''BinTree 1000
```

which makes `BinTree` an instance of the `BoltzmannSampler` type class:

``` hs
class BoltzmannSampler a where
  -- |
  --  Samples a random object of type @a@. If the object size is larger than
  --  the given upper bound parameter, @Nothing@ is returned instead.
  sample :: RandomGen g => Int -> MaybeT (BuffonMachine g) (a, Int)
```

The so created `sample` function implements a Boltzmann sampler for `BinTree`.
The sampler outcomes follow a distribution in which *all* binary trees of equal
size have the *exact same* (uniform) probability of being sampled. The *size* of
the outcomes is itself a random variable with the user-declared mean value of
`1000`.

Further control over the outcome size distribution can be achieved through
*rejection sampling* discarding objects falling outside of the admissible lower
and upper size bounds.

``` hs
rejectionSampler ::
  (RandomGen g, BoltzmannSampler a) => LowerBound -> UpperBound -> BuffonMachine g a
```

The `BuffonMachine g a` type encapsulates computations using random bits and is
in that respect closely related to QuickCheck's generator type `Gen a` to which
it can be converted through, e.g.:

``` hs
quickCheckRejectionSampler ::
  BoltzmannSampler a => (Int -> (LowerBound, UpperBound)) -> Gen a
```

### Features

#### Multiparametric samplers

In a more advanced setting, `generic-boltzmann-brain` supports *inter allia*:
- systems of, possibly mutually recursive, algebraic data types,
- custom size definitions through user-declared constructor weights,
- outcome distribution tuning through constructors with user-prescribed frequencies.

Consider the following example of lambda terms in [DeBruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_notation):

``` hs
data DeBruijn
  = Z
  | S DeBruijn
  deriving (Show)

data Lambda
  = Index DeBruijn
  | App Lambda Lambda
  | Abs Lambda
  deriving (Show)
```

Suppose that we want a size notion where all constructors have weight
`1` (*i.e.* contribute one to the overall term size) *except* for `Index` which,
as a simple type converter, should contribute no size. Furthermore, suppose that
we want to *skew* the default uniform distribution, and increase the expected number
of abstractions in a random lambda term while retaining the sampler's *fairness*, *i.e.*
lambda terms of equal size *and* equal number of abstractions should have the same
probability of being sampled. With `generic-boltzmann-brain` we can generate such
a sampler as follows:

``` hs
mkBoltzmannSampler
  System
    { targetType = ''Lambda
    , meanSize = 10_000
    , frequencies = ('Abs, 4_000) <:> def
    , weights =
        ('Index, 0)
          <:> $(mkDefWeights ''Lambda)
    }
```

Note that `mkDefWeights` generates default constructor weights, whereas `<:>`
*overrides* the default value of `('Index, 1)` to be `('Index, 0)`. Likewise,
`def` defines the default (empty) set of frequencies and `<:>` includes
the custom frequency for the `Abs` constructor.

Using constructor tuning, it is therefore possible to distort the natural
frequency of each constructor in the given system. However, such an additional
non-trivial tuning procedure causes a not insignificant change in the underlying
probability model. In extreme cases, such as for instance requiring *80%* of
internal nodes in plane binary trees, the sampler might be unavailable or
virtually ineffective due to the sparsity of tuned structures.

**Please tune with caution!**

#### Multiple samplers for the same types

It is possible to have multiple samplers for the same underlying type, for
instance if different constructor frequencies are needed or different size
notions are required. Simply create a `newtype` wrapper and generate a let
`generic-boltzmann-brain` generate a new instance of `BoltzmannSampler`:

``` hs
newtype BinLambda = MkBinLambda Lambda
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinLambda
    , meanSize = 6_000
    , frequencies = ('Abs, 2340) <:> def
    , weights =
        ('Index, 0)
          <:> ('App, 2)
          <:> ('Abs, 2)
          <:> $(mkDefWeights ''Lambda)
    }
```

### Installation
Currently, no pre-compiled binaries are available.

`generic-boltzmann-brain` uses an external [Python](https://www.python.org/) library
called [Paganini](https://github.com/maciej-bendkowski/paganini) to do the construction
of Boltzmann samplers. `Python` with available `paganini` are expected to be executable
and present in the `PATH`.

We recommend using `stack` for compiling `generic-boltzmann-brain` sources.

### Known limitations

- Polymorphic data types are *not* supported.
- Non-algebraic data type specifications are currently *not* supported, though
  some of their extensions, such as certain *Pólya structures* are *feasible*.
- Nested lists such as `[[Lambda]]` are *not* supported, even though lists such
  as `[Lambda]` are. Note however that this is not a conceptual limitation, and
  could be supported in future versions of `generic-boltzmann-brain`.

### Related work

As such, *generic-boltzmann-brain* is a twin project of
[Boltzmann-brain](https://github.com/maciej-bendkowski/boltzmann-brain). If you
are interested in generating standalone samplers out of a textual
representation, consider using *boltzmann-brain* instead.

`generic-boltzmann-brain` heavily relies on published work of numerous excellent
authors. Below, you can find a short (and definitely inexhaustive) list of
papers on the subject:

- [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for
   the random generation of combinatorial
structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
- [O.Bodini, J. Lumbroso, N. Rolin: Analytic samplers and the combinatorial rejection method](https://dl.acm.org/citation.cfm?id=2790220&dl=ACM&coll=DL)
- [F. Saad, C. Freer, M. Rinard, V. Mansinghka : Optimal Approximate Sampling from Discrete Probability Distributions](https://arxiv.org/pdf/2001.04555.pdf)
- [M. Bendkowski, S. Dovgal, O. Bodini : Polynomial tuning of multiparametric combinatorial
samplers](https://epubs.siam.org/doi/10.1137/1.9781611975062.9)

# fused-effects-mwc-random

[![Hackage](https://img.shields.io/hackage/v/fused-effects-mwc-random.svg?logo=haskell)](https://hackage.haskell.org/package/fused-effects-mwc-random)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

High-quality uniformly-distributed random number generation as an effect.

This library provides a `Random` effect that piggybacks atop the `mwc-random` package, which produces high-quality random numbers in efficient time.

For more advanced users, there exists also a module (`Control.Carrier.Random.Instances`) that provides the orphan instances required to use most `fused-effect` stacks with the `PrimMonad` machinery that powers `mwc-random`. This module is most useful when porting monad transformer stacks that already use the @mwc-random@ API, fine-grained need to control the behavior or state of a random number generator.

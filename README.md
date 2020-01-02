# fused-effects-mwc-random

[![Hackage](https://img.shields.io/hackage/v/fused-effects-mwc-random.svg?logo=haskell)](https://hackage.haskell.org/package/fused-effects-mwc-random)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

High-quality uniformly-distributed pseudorandom number generation as an effect.

This library provides a `Random` effect that piggybacks atop the `mwc-random` package, which produces high-quality random numbers in efficient time. A carrier type (`Control.Carrier.Random.Lifted`) encapsulates generator state and provides a convenient API to invoke system random number generation or pass a predefined seed. The `mwc-random` package is finely tuned; as such, this package should be an industrial-strength source of random numbers, suitable for tight loops.

Please note that the [MWC-256](https://en.wikipedia.org/wiki/Multiply-with-carry_pseudorandom_number_generator) algorithm that underlies `mwc-random` is *not* cryptographically secure and should not be used for such purposes. Crypto code should use [`cryptonite`](https://hackage.haskell.org/package/cryptonite), as I'm sure you already know.

For more advanced users, there exists also a module (`Control.Carrier.Random.Instances`) that provides the orphan instances required to use most `fused-effect` stacks with the `PrimMonad` machinery that powers `mwc-random`. This module is most useful when porting monad transformer stacks that already use the `mwc-random` API, or if you need to drop into the native `mwc-random` API.

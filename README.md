# fused-effects-mwc-random

[![Hackage](https://img.shields.io/hackage/v/fused-effects-mwc-random.svg?logo=haskell)](https://hackage.haskell.org/package/fused-effects-mwc-random)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

High-quality uniformly-distributed random number generation as an effect.

This library provides a `Random` effect that piggybacks atop the `mwc-random` package, which produces high-quality random numbers in efficient time.

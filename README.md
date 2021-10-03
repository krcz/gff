# gff

**work in progress**

`gff` builds on [ff](https://github.com/zkcrypto/ff) library to provide support for non-prime Galois (/Finite) Fields, i.e. fields with `p^m` elements where `p` is prime and `m >= 2` is an integer (please note it is very different from `Z_{p^m}` ring, i.e. modular arithmetics mod `p^m`)

## Roadmap

v0.1:

* implement full `Field` trait from `ff` crate
* Frobenius morphism implementation
* polynomial basis only

Further versions:

* normal bases (including optimal normal bases)
* use `u64` as underlying representation instead of `ff`-derived structs for small primes

## Use example

Please refer to [tests](tests/derive.rs) to see how to use the macro.

## License

[MIT License](LICENSE.md)

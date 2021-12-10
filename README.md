# gff

**work in progress**

`gff` builds on [ff](https://github.com/zkcrypto/ff) library to provide support for non-prime Galois (/Finite) Fields, i.e. fields with `p^m` elements where `p` is prime and `m >= 2` is an integer (please note it is very different from `Z_{p^m}` ring, i.e. modular arithmetics mod `p^m`)

## Roadmap

v0.1:

* implement full `Field` trait from `ff` crate (done)
* Frobenius morphism implementation (done)
* polynomial basis only
* sqrt implementation
* polishing the code

Further versions:

* faster Frobenius morphism using precomputation
* optimal extension fields (for fast Frobenius morphism and multiplication)
* normal bases (including optimal normal bases)
* use `u64` as underlying representation instead of `ff`-derived structs for small primes

## Use example

Please refer to [tests](tests/derive.rs) to see how to use the macro.

## License

[MIT License](LICENSE.md)

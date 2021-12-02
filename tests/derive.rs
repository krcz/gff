#[macro_use]
extern crate ff;

#[macro_use]
extern crate gff;

use ::ff::Field;
use proptest::prelude::*;
use std::convert::TryInto;

#[derive(PrimeField)]
#[PrimeFieldModulus = "65537"]
#[PrimeFieldGenerator = "3"]
#[PrimeFieldReprEndianness = "little"]
struct Fp([u64; 1]);

#[derive(LargePrimeExtensionField)]
#[ExtensionDegree = 4]
#[MinimalPolynomial = "x^4 + 7 x^2 + 48035 x + 3"]
struct GF([Fp; 4]);

prop_compose! {
    fn any_fp()(i in 0..65537) -> Fp {
        Fp([i as u64])
    }
}

prop_compose! {
    fn any_gf()(v in prop::collection::vec(any_fp(), 4)) -> GF {
        let a: [Fp; 4] = v.try_into().unwrap();
        GF(a)
    }
}

proptest! {
    #[test]
    fn double_negation(ref a in any_gf()) {
        assert_eq!(&-(-a.clone()), a);
    }

    #[test]
    fn zero_is_additive_identity(ref a in any_gf()) {
        assert_eq!(a.clone() + GF::zero(), a.clone());
        assert_eq!(GF::zero() + a.clone(), a.clone());
    }

    #[test]
    fn addition_commutative(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(a.clone() + b.clone(), b.clone() + a.clone());
    }

    #[test]
    fn addition_commutative_ref(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(a.clone() + b, b.clone() + a);
    }

    #[test]
    fn subtraction_addition_cancel_out(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(&(a.clone() - b + b), a);
    }

    #[test]
    fn subtraction_is_negation_addition(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(&(a.clone() - b), &(a.clone() + (-b.clone())));
    }

    #[test]
    fn zero_is_multiplicative_annihilator(ref a in any_gf()) {
        assert_eq!(a.clone() * GF::zero(), GF::zero());
        assert_eq!(GF::zero() * a.clone(), GF::zero());
    }

    #[test]
    fn one_is_multiplicative_identity(ref a in any_gf()) {
        assert_eq!(a.clone() * GF::one(), a.clone());
        assert_eq!(GF::one() * a.clone(), a.clone());
    }

    #[test]
    fn multiplication_commutative(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(a.clone() * b.clone(), b.clone() * a.clone());
    }

    #[test]
    fn multiplication_distributive(ref a in any_gf(), ref b in any_gf(), ref c in any_gf()) {
        assert_eq!(a.clone() * (b.clone() + c.clone()), a.clone() * b.clone() + a.clone() * c.clone());
    }

    #[test]
    fn square_of_negation(ref a in any_gf()) {
        let na = -a.clone();
        assert_eq!(na.clone() * na.clone(), a.clone() * a.clone());
    }
}

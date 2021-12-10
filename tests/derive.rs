#[macro_use]
extern crate ff;

#[macro_use]
extern crate gff;

use ::ff::Field;
use gff::GenericFiniteField;
use proptest::prelude::*;
use std::convert::TryInto;

#[derive(PrimeField)]
#[PrimeFieldModulus = "65537"]
#[PrimeFieldGenerator = "3"]
#[PrimeFieldReprEndianness = "little"]
struct Fp([u64; 1]);

#[derive(LargePrimeExtensionField)]
#[PrimeFieldModulus = "65537"]
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
        assert_eq!(na.clone().square(), a.clone().square());
    }

    #[test]
    fn square_is_multiplication_by_self(ref a in any_gf()) {
        assert_eq!(a.clone().square(), a.clone() * a.clone());
    }

    #[test]
    fn element_times_inverse_is_one(ref a in any_gf()) {
        if a != &GF::zero() {
            assert_eq!(a.clone() * a.clone().invert().unwrap(), GF::one());
        }
    }

    #[test]
    fn frobenius_is_additive(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(a.clone().frobenius(1) + b.clone().frobenius(1), (*a + b).frobenius(1));
    }

    #[test]
    fn frobenius_is_multiplicative(ref a in any_gf(), ref b in any_gf()) {
        assert_eq!(a.clone().frobenius(1) * b.clone().frobenius(1), (*a * b).frobenius(1));
    }

    #[test]
    fn all_additions_equal(ref a in any_gf(), ref b in any_gf()) {
        let r1 = a.clone() + b.clone();
        let r2 = a.clone() + b;
        let mut r3 = a.clone();
        r3 += b.clone();
        let mut r4 = a.clone();
        r4 += b;

        assert_eq!(r2, r1);
        assert_eq!(r3, r1);
        assert_eq!(r4, r1);
    }

    #[test]
    fn all_subtractions_equal(ref a in any_gf(), ref b in any_gf()) {
        let r1 = a.clone() - b.clone();
        let r2 = a.clone() - b;
        let mut r3 = a.clone();
        r3 -= b.clone();
        let mut r4 = a.clone();
        r4 -= b;

        assert_eq!(r2, r1);
        assert_eq!(r3, r1);
        assert_eq!(r4, r1);
    }

    #[test]
    fn all_multiplications_equal(ref a in any_gf(), ref b in any_gf()) {
        let r1 = a.clone() * b.clone();
        let r2 = a.clone() * b;
        let mut r3 = a.clone();
        r3 *= b.clone();
        let mut r4 = a.clone();
        r4 *= b;

        assert_eq!(r2, r1);
        assert_eq!(r3, r1);
        assert_eq!(r4, r1);
    }
}
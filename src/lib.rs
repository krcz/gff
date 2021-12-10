#[cfg(feature = "derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub use gff_derive::LargePrimeExtensionField;

use ff::Field;

#[cfg(feature = "derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub mod derive {
    pub use {rand_core, num_bigint, subtle};
}

pub trait GenericFiniteField: Field {
    /// Returns k-iterated Frobenius morphism, i.e. exponentiation to p^k
    fn frobenius(self, k: u64) -> Self;
}

pub trait LargePrimeExtensionField: GenericFiniteField {}

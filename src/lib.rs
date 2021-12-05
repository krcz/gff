#[cfg(feature = "derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub use gff_derive::LargePrimeExtensionField;

use core::ops::Neg;
use ff::Field;

#[cfg(feature = "derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub mod derive {
    pub use num_bigint;
}

pub trait GenericFiniteField: Field {
    fn multiplicative_generator() -> Self;

    fn frobenius_morphism(&self) -> Self;
}

pub trait LargePrimeExtensionField: Sized + Clone + Neg<Output = Self> {}

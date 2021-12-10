extern crate proc_macro;
extern crate proc_macro2;

use quote::quote;
use regex::Regex;
use std::iter;
use core::str::FromStr;
use num_bigint::BigUint;

fn fetch_attr(name: &str, attrs: &[syn::Attribute]) -> Option<syn::Lit> {
    for attr in attrs {
        if let Ok(meta) = attr.parse_meta() {
            match meta {
                syn::Meta::NameValue(nv) => {
                    if nv.path.get_ident().map(|i| i.to_string()) == Some(name.to_string()) {
                        return Some(nv.lit);
                    }
                }
                _ => {
                    panic!("unrecognized token")
                }
            }
        }
    }

    None
}

fn parse_monic_polynomial(v: &str, m: usize) -> Vec<u64> {
    if v.contains("-") {
        panic!("Negative coefficients in minimal polynomial are not supported")
    }
    let POLY_PLUS_RE: Regex = Regex::new(r"\s*\+\s*").unwrap();
    let MONOMIAL_RE: Regex = Regex::new(r"^([1-9][0-9]*)?\s*(x(?:\^([1-9][0-9]*))?)?$").unwrap();

    let elements: Vec<&str> = POLY_PLUS_RE.split(v).collect();

    if elements.first() != Some(&format!("x^{}", m).as_str()) {
        panic!(
            "First element must be x^{}, not {}",
            m,
            elements.first().unwrap_or(&"")
        );
    }

    let mut res = vec![0u64; m];

    let mut last_power = m;

    for el in &elements[1..] {
        let caps = MONOMIAL_RE
            .captures(el)
            .expect(&format!("Invalid monomial: {}", el));
        let coeff: u64 = caps.get(1).map_or(1, |m| {
            m.as_str()
                .parse()
                .expect("Coefficients must be valid u64 numbers")
        });
        let power: usize = caps.get(2).map_or(0, |_| {
            caps.get(3)
                .map_or(1, |k| k.as_str().parse().expect("Powers must be numbers"))
        });
        if power >= last_power {
            panic!(
                "Polynomial elements need to be listed with decreasing powers, {} has lower one",
                el
            );
        }
        res[power] = coeff;
        last_power = power;
    }

    res
}

/// Derive the `LargePrimeExtensionField` trait
#[proc_macro_derive(
    LargePrimeExtensionField,
    attributes(ExtensionDegree, MinimalPolynomial, PrimeFieldModulus)
)]
pub fn large_prime_extension_field(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the type definition
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let degree: usize = match fetch_attr("ExtensionDegree", &ast.attrs)
        .expect("Please supply an ExtensionDegree attribute")
    {
        syn::Lit::Int(ref i) => i.base10_parse().expect("ExtensionDegree must be a number"),
        _ => panic!("ExtensionDegree must be a number"),
    };

    let prime_modulus_str: String = match fetch_attr("PrimeFieldModulus", &ast.attrs)
        .expect("Please supply a PrimeFieldModulus attribute")
    {
        syn::Lit::Str(ref s) => s.value(),
        _ => panic!("PrimeFieldModulus must be a string"),
    };

    let prime_field_modulus = BigUint::from_str(&prime_modulus_str).expect("Couldn't parse PrimeFieldModulus");

    let min_poly_str: String = match fetch_attr("MinimalPolynomial", &ast.attrs)
        .expect("Please supply a MinimalPolynomial attribute")
    {
        syn::Lit::Str(ref s) => s.value(),
        _ => panic!("MinimalPolynomial must be a string"),
    };

    let min_poly = parse_monic_polynomial(min_poly_str.as_str(), degree);

    let struct_ast = match &ast.data {
        syn::Data::Struct(x) => x,
        _ => {
            return syn::Error::new_spanned(ast, "Xxx only works for structs.")
                .to_compile_error()
                .into()
        }
    };

    let field = match &struct_ast.fields {
        syn::Fields::Unnamed(x) if x.unnamed.len() == 1 => &x.unnamed[0],
        _ => {
            return syn::Error::new_spanned(
                &ast.ident,
                "The struct must contain array of coefficients.",
            )
            .to_compile_error()
            .into()
        }
    };

    let arr = match &field.ty {
        syn::Type::Array(x) => x,
        _ => {
            return syn::Error::new_spanned(field, "The struct must contain array of coefficients.")
                .to_compile_error()
                .into()
        }
    };

    let fp_name_opt = match arr.elem.as_ref() {
        syn::Type::Path(path) => path.path.get_ident(),
        _ => None,
    };

    let fp_name = match fp_name_opt {
        Some(indent) => indent,
        None => {
            return syn::Error::new_spanned(
                &arr.elem,
                "The struct array elements must represent underlying prime field",
            )
            .to_compile_error()
            .into()
        }
    };

    let mut gen = proc_macro2::TokenStream::new();

    gen.extend(large_prime_extension_field_impl(
        &ast.ident, &fp_name, degree, &prime_field_modulus, min_poly,
    ));

    gen.into()
}

fn large_prime_extension_field_impl(
    name: &syn::Ident,
    fp_name: &syn::Ident,
    degree: usize,
    prime_field_modulus: &BigUint,
    min_poly: Vec<u64>,
) -> proc_macro2::TokenStream {
    let double_degree_minus_one = 2 * degree - 1;

    let min_poly_repr = quote! { [#(#fp_name([#min_poly])),*] };
    let zero_poly_repr = quote! { [#fp_name::zero(); #degree] };
    let one_poly_repr_elements = iter::repeat(quote! { #fp_name::zero() }).take(degree - 1);
    let one_poly_repr = quote! { [#fp_name::one(), #(#one_poly_repr_elements),*] };

    let prime_modulus_u32_digits = prime_field_modulus.to_u32_digits();
    let random_fp_array = vec![quote!{#fp_name::random(&mut rng)}; degree];

    quote! {

        use ::gff::derive::subtle::ConditionallySelectable;

        const M: usize = #degree;

        const MIN_POLY: [#fp_name; #degree] = #min_poly_repr;

        fn PRIME_FIELD_MODULUS() -> ::gff::derive::num_bigint::BigUint {
            ::gff::derive::num_bigint::BigUint::new(vec![#(#prime_modulus_u32_digits),*])
        } 

        impl ::core::marker::Copy for #name { }

        impl ::core::clone::Clone for #name {
            fn clone(&self) -> #name {
                *self
            }
        }

        impl ::core::cmp::PartialEq for #name {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl ::core::cmp::Eq for #name { }

        impl ::gff::LargePrimeExtensionField for #name {}

        impl ::core::fmt::Debug for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                write!(f, "{}{:?}", stringify!(#name), self.0)
            }
        }

        impl Default for #name {
            fn default() -> Self {
                #name::zero()
            }
        }

        impl ::gff::derive::subtle::ConditionallySelectable for #name {
            fn conditional_select(a: &Self, b: &Self, choice: ::gff::derive::subtle::Choice) -> Self {
                let mut arr = [#fp_name::default(); #degree];
                for i in 0..#degree {
                    arr[i] = #fp_name::conditional_select(&a.0[i], &b.0[i], choice);
                }
                #name(arr)
            }
        }

        impl ::gff::derive::subtle::ConstantTimeEq for #name {
            fn ct_eq(&self, other: &Self) -> ::gff::derive::subtle::Choice {
                let mut acc = self.0[0].ct_eq(&other.0[0]);
                for i in 1..#degree {
                    acc &= self.0[i].ct_eq(&other.0[i]);
                }
                acc
            }
        }

        impl #name {
            fn powx(mut self, e: ::gff::derive::num_bigint::BigUint) -> Self {
                let mut acc = Self::one();
                for i in 0..e.bits() {
                    if e.bit(i) {
                        acc = acc * self;
                    }
                    self = self.square();
                }
                self = acc;
                self
            }

            #[inline]
            fn reduce(p: &mut [#fp_name; #double_degree_minus_one]) {
                for i in (M .. (2 * M - 1)).rev() {
                    for j in 0 .. M {
                        p[i - M + j] -= p[i] * MIN_POLY[j];
                    }
                    p[i] = #fp_name::zero();
                }
            }

        }

        impl gff::GenericFiniteField for #name {
            fn frobenius(mut self, s: u64) -> #name {
                self.powx(PRIME_FIELD_MODULUS().pow((s % (M as u64)) as u32))
            }
        }

        impl ff::Field for #name {
            fn zero() -> Self {
                #name(#zero_poly_repr)
            }

            fn one() -> Self {
                #name(#one_poly_repr)
            }

            fn random(mut rng: impl ::gff::derive::rand_core::RngCore) -> Self {
                #name([#(#random_fp_array),*])
            }

            fn double(&self) -> #name {
                let mut arr = [#fp_name::default(); #degree];
                for i in 0..#degree {
                    arr[i] = self.0[i].double();
                }
                #name(arr)
            }

            fn square(&self) -> #name {
                let mut res: [#fp_name; #double_degree_minus_one] = [#fp_name::zero(); #double_degree_minus_one];
                for i in 0..M {
                    res[2 * i] += self.0[i].square();

                    for j in (i + 1)..M {
                        res[i + j] += (self.0[i] * self.0[j]).double()
                    }
                }
                Self::reduce(&mut res);
                let mut arr = [#fp_name::default(); #degree];
                for i in 0..M {
                    arr[i] = res[i];
                }
                #name(arr)
            }

            fn invert(&self) -> ::gff::derive::subtle::CtOption<Self> {
                let res = self.powx(PRIME_FIELD_MODULUS().pow(M as u32) - 2u8);
                ::gff::derive::subtle::CtOption::new(res, !self.is_zero())
            }

            fn sqrt(&self) -> ::gff::derive::subtle::CtOption<Self> {
                panic!("Square root is not implemented")
            }
        }

        impl ::core::ops::Neg for #name {
            type Output = Self;

            #[inline]
            fn neg(mut self) -> Self {
                for i in 0..M {
                    self.0[i] = -self.0[i]
                }
                self
            }
        }

        impl ::core::ops::Add<&#name> for #name {
            type Output = #name;

            #[inline]
            fn add(mut self, other: &Self) -> #name {
                for i in 0..M {
                    self.0[i] += other.0[i]
                }
                self
            }
        }

        impl ::core::ops::Add for #name {
            type Output = #name;

            #[inline]
            fn add(self, other: Self) -> #name {
                self + &other
            }
        }

        impl ::core::ops::Sub<&#name> for #name {
            type Output = #name;

            #[inline]
            fn sub(mut self, other: &Self) -> #name {
                for i in 0..M {
                    self.0[i] -= other.0[i]
                }
                self
            }
        }

        impl ::core::ops::Sub for #name {
            type Output = #name;

            #[inline]
            fn sub(self, other: Self) -> #name {
                self - &other
            }
        }

        impl ::core::ops::Mul<&#name> for #name {
            type Output = #name;

            fn mul(mut self, other: &Self) -> #name {
                let mut res: [#fp_name; #double_degree_minus_one] = [#fp_name::zero(); #double_degree_minus_one];
                for i in 0..M {
                    for j in 0..M {
                        res[i + j] += self.0[i] * other.0[j]
                    }
                }
                Self::reduce(&mut res);
                for i in 0..M {
                    self.0[i] = res[i];
                }
                self
            }
        }

        impl ::core::ops::Mul for #name {
            type Output = #name;

            #[inline]
            fn mul(self, other: Self) -> #name {
                self * &other
            }
        }

        impl ::core::ops::AddAssign for #name {
            fn add_assign(&mut self, rhs: Self) {
                *self = *self + rhs;
            }
        }

        impl ::core::ops::SubAssign for #name {
            fn sub_assign(&mut self, rhs: Self) {
                *self = *self - rhs;
            }
        }

        impl ::core::ops::MulAssign for #name {
            fn mul_assign(&mut self, rhs: Self) {
                *self = *self * rhs;
            }
        }

        impl<'a> ::core::ops::AddAssign<&'a Self> for #name {
            fn add_assign(&mut self, rhs: &'a Self) {
                *self = *self + rhs;
            }
        }

        impl<'a> ::core::ops::SubAssign<&'a Self> for #name {
            fn sub_assign(&mut self, rhs: &'a Self) {
                *self = *self - rhs;
            }
        }

        impl<'a> ::core::ops::MulAssign<&'a Self> for #name {
            fn mul_assign(&mut self, rhs: &'a Self) {
                *self = *self * rhs;
            }
        }
    }
}

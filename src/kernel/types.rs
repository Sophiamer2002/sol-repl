use std::rc::Rc;

use num_bigint::BigInt;

use crate::utils::fraction::Fraction;

pub type Ty = Rc<Type>;
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    B(BasicType),
    C(CompoundType),
    NumLiteral(Fraction),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BasicType {
    Bool(),
    Int(u16),
    Uint(u16),
    Bytes(u8),
    Address(bool), // payable or not
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CompoundType {
    String,
    DynamicBytes,
    Struct {
        identifier: String,
        fields: Vec<(String, Ty)>
    },
    Mapping {
        key: Rc<Type>,  // shall be a basic type
        value: Rc<Type>
    },
    Array {
        len: Option<u32>,
        value: Rc<Type>
    },
}

impl Type {
    pub fn implicit_conversion(from: &Ty, to: &Ty) -> bool {
        match (&**from, &**to) {
            (Type::B(BasicType::Int(sz1)), Type::B(BasicType::Int(sz2))) => sz1 <= sz2,
            (Type::B(BasicType::Uint(sz1)), Type::B(BasicType::Uint(sz2))) => sz1 <= sz2,
            (Type::B(BasicType::Bytes(sz1)), Type::B(BasicType::Bytes(sz2))) => sz1 <= sz2,
            (Type::B(BasicType::Address(payable1)), Type::B(BasicType::Address(payable2))) => !payable2 || *payable1,
            (Type::B(BasicType::Bool()), Type::B(BasicType::Bool())) => true,
            (Type::C(CompoundType::String), _) => unimplemented!(),
            (Type::C(CompoundType::DynamicBytes), _) => unimplemented!(),
            (Type::C(CompoundType::Struct { identifier: id1, .. }),
                Type::C(CompoundType::Struct { identifier: id2, .. })) => id1 == id2,
            (Type::C(CompoundType::Mapping { .. }), _) => false,
            (Type::C(CompoundType::Array { len: len1, value: value1 }),
                Type::C(CompoundType::Array { len: len2, value: value2 })) => {
                let x = match (len1, len2) {
                    (Some(len1), _) => *len1 <= len2.unwrap_or(*len1),
                    (None, Some(_)) => false,
                    (None, None) => true,
                };
                x && Type::implicit_conversion(value1, value2)
            },
            (Type::NumLiteral(num), Type::B(BasicType::Int(sz))) => num.is_integer() && bits_need(&num.numerator, true) <= *sz,
            (Type::NumLiteral(num), Type::B(BasicType::Uint(sz))) => num.is_non_negative_integer() && bits_need(&num.numerator, false) <= *sz,
            _ => false
        }
    }

    pub fn sanity_check(&self) {
        match self {
            Type::B(BasicType::Int(sz)) => {
                assert!(*sz <= 256 && *sz > 0 && *sz & 7 == 0);
            },
            Type::B(BasicType::Uint(sz)) => {
                assert!(*sz <= 256 && *sz > 0 && *sz & 7 == 0);
            },
            Type::B(BasicType::Bytes(sz)) => {
                assert!(*sz <= 32 && *sz > 0);
            },
            Type::B(BasicType::Address(_)) => {},
            Type::B(BasicType::Bool()) => {},
            Type::C(CompoundType::String) => unimplemented!(),
            Type::C(CompoundType::DynamicBytes) => unimplemented!(),
            Type::C(CompoundType::Struct { fields, .. }) => {
                for (_, ty) in fields {
                    if matches!(**ty, Type::NumLiteral(_)) {
                        panic!("Struct field cannot be a number literal");
                    }
                    ty.sanity_check();
                }
            },
            Type::C(CompoundType::Mapping { key, value }) => {
                match &**key {
                    Type::B(_) => {},
                    _ => panic!("Mapping key must be a basic type"),
                }
                if matches!(**value, Type::NumLiteral(_)) {
                    panic!("Mapping value cannot be a number literal");
                }
                key.sanity_check();
                value.sanity_check();
            },
            Type::C(CompoundType::Array { value, .. }) => {
                if matches!(**value, Type::NumLiteral(_)) {
                    panic!("Array value cannot be a number literal");
                }
                value.sanity_check();
            },
            Type::NumLiteral(_) => {},
        }
    }

    pub fn deduct_common_types(tys: Vec<Ty>) -> Result<Ty, String> {
        if tys.is_empty() {
            return Err("Empty type list".to_string());
        }

        // case 1: if exists compound type, then all types must be the same
        let compound = tys.iter().any(|ty| matches!(&**ty, Type::C(_)));
        if compound {
            for ty in &tys {
                if tys[0] != *ty {
                    return Err("Cannot deduct common types: Compound type must be the same".to_string());
                }
            }

            return Ok(tys[0].clone());
        } else {
            // case 2: if all are basic types, then the result is the largest one
            if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Bool())))) {
                return Ok(Rc::new(Type::B(BasicType::Bool())));
            }

            for i in (8..=256).step_by(8) {
                if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Uint(i))))) {
                    return Ok(Rc::new(Type::B(BasicType::Uint(i))));
                }

                if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Int(i))))) {
                    return Ok(Rc::new(Type::B(BasicType::Int(i))));
                }
            }

            for i in 1..=32 {
                if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Bytes(i))))) {
                    return Ok(Rc::new(Type::B(BasicType::Bytes(i))));
                }
            }

            if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Address(false))))) {
                return Ok(Rc::new(Type::B(BasicType::Address(true))));
            }

            if tys.iter().all(|t| Type::implicit_conversion(t, &Rc::new(Type::B(BasicType::Address(true))))) {
                return Ok(Rc::new(Type::B(BasicType::Address(true))));
            }
        }

        Err("Cannot deduct common types".to_string())
    }
}

pub fn bits_need(num: &BigInt, signed: bool) -> u16 {
    // TODO: num might be too big
    //       or not consistent with the signed flag.
    let mut bits = num.bits();
    if num < &BigInt::from(0) && signed {
        bits = (-num.clone() - BigInt::from(1)).bits() + 1;
    }

    ((bits + 7) & (!7)) as u16
}
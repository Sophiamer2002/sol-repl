use std::fmt::{Display, Formatter};
use crate::utils::fraction::Fraction;

use super::prelude::*;

use num_bigint::BigInt;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Data {
    Bool(Bool),
    Int(Int),
    Uint(Uint),
    Bytes(Bytes),
    Address(Address),
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Bool(b) => write!(f, "{}", b.value),
            Data::Int(i) => write!(f, "{}", i.value),
            Data::Uint(u) => write!(f, "{}", u.value),
            Data::Bytes(b) => write!(f, "{:?}", b.value),
            Data::Address(a) => write!(f, "{:?}", a.value),
        }
    }
}

impl Data {
    pub fn match_type_exact(&self, ty: &BasicType) -> bool {
        match self {
            Data::Bool(_) => matches!(ty, BasicType::Bool()),
            Data::Int(Int{size, ..}) => matches!(ty, BasicType::Int(sz) if sz == size),
            Data::Uint(Uint{size, ..}) => matches!(ty, BasicType::Uint(sz) if sz == size),
            Data::Bytes(Bytes{size, ..}) => matches!(ty, BasicType::Bytes(sz) if sz == size),
            Data::Address(Address { payable, .. }) => matches!(ty, BasicType::Address(p) if p == payable),
        }
    }

    // check if self(data) can be converted to ty implicitly.
    pub fn match_type_auto(&self, ty: &BasicType) -> bool {
        match self {
            Data::Bool(_) => matches!(ty, BasicType::Bool()),
            Data::Int(Int{size, ..}) => matches!(ty, BasicType::Int(sz) if sz <= size),
            Data::Uint(Uint{size, ..}) => matches!(ty, BasicType::Uint(sz) if sz <= size),
            Data::Bytes(Bytes{size, ..}) => matches!(ty, BasicType::Bytes(sz) if sz <= size),
            Data::Address(Address { payable, .. }) => matches!(ty, BasicType::Address(p) if *p || !payable),
        }
    }

    pub fn default(ty: &BasicType) -> Self {
        match ty {
            BasicType::Bool() => Data::Bool(Bool { value: false }),
            BasicType::Int(size) => Data::Int(Int { size: *size, value: BigInt::from(0) }),
            BasicType::Uint(size) => Data::Uint(Uint { size: *size, value: BigInt::from(0) }),
            BasicType::Bytes(size) => Data::Bytes(Bytes { size: *size, value: vec![0; *size as usize] }),
            BasicType::Address(true) => Data::Address(Address { payable: true, value: [0; 20] }),
            BasicType::Address(false) => Data::Address(Address { payable: false, value: [0; 20] }),
        }
    }
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Int {
    pub size: u16, // in bits
    pub value: BigInt,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Uint {
    pub size: u16, // in bits.
    pub value: BigInt,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bool {
    pub value: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bytes {
    pub size: u8,
    pub value: Vec<u8>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Address {
    pub payable: bool,
    pub value: [u8; 20],
}

// #[derive(Clone, Debug, Hash, PartialEq, Eq)]
// pub struct StringQ {
//     pub value: String,
// }

/// see https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.userDefinableOperator
pub trait Operator {
    fn bitwise_and(&mut self, rhs: &Data) -> Result<Data, String>;

    fn bitwise_not(&mut self) -> Result<Data, String>;

    fn bitwise_or(&mut self, rhs: &Data) -> Result<Data, String>;

    fn bitwise_xor(&mut self, rhs: &Data) -> Result<Data, String>;

    fn add(&mut self, rhs: &Data) -> Result<Data, String>;

    fn divide(&mut self, rhs: &Data) -> Result<Data, String>;

    fn multiply(&mut self, rhs: &Data) -> Result<Data, String>;

    fn modulo(&mut self, rhs: &Data) -> Result<Data, String>;

    fn subtract(&mut self, rhs: &Data) -> Result<Data, String>;

    fn shift_left(&mut self, rhs: &Data) -> Result<Data, String>;

    fn shift_right(&mut self, rhs: &Data) -> Result<Data, String>;

    fn equal(&mut self, rhs: &Data) -> Result<Data, String>;

    fn more(&mut self, rhs: &Data) -> Result<Data, String>;

    fn more_or_equal(&mut self, rhs: &Data) -> Result<Data, String>;

    fn less(&mut self, rhs: &Data) -> Result<Data, String>;

    fn less_or_equal(&mut self, rhs: &Data) -> Result<Data, String>;

    fn not_equal(&mut self, rhs: &Data) -> Result<Data, String>;
}

pub trait Number {
    fn max_value(size: u16) -> BigInt;
    fn min_value(size: u16) -> BigInt;
}

pub trait DefaultValue {
    fn default_value(size: Option<u8>) -> Self;
}

impl Int {
    pub fn new_from_literal(value: BigInt, size: Option<u16>) -> Result<Self, String> {
        let vv = if value >= BigInt::from(0) {
            value.clone()
        } else {
            -value.clone() - 1
        };
        let sz_need = (vv.bits() + 15) - (vv.bits() + 15) % 8;
        if sz_need <= size.unwrap_or(256) as u64 {
            Ok(Int {
                size: size.unwrap_or(sz_need as u16),
                value,
            })
        } else {
            Err("int literal too large".to_string())
        }
    }

    pub fn op_assign(&mut self, rhs: &Data, op: fn(&BigInt, &BigInt) -> BigInt) -> Result<Data, String> {
        if let Data::Int(rhs) = rhs {
            let sz = self.size.max(rhs.size);
            let value = op(&self.value, &rhs.value);

            if value > Self::max_value(sz) || value < Self::min_value(sz) {
                return Err("overflow".to_string());
            }

            return Ok(Data::Int(Int {
                size: sz,
                value,
            }));
        }

        Err("type not match".to_string())
    }
}

impl Uint {
    pub fn new_from_literal(value: BigInt, size: Option<u16>) -> Result<Self, String> {
        let sz_need = (value.bits() + 7) - (value.bits() + 7) % 8;
        if sz_need <= size.unwrap_or(256) as u64 {
            Ok(Uint {
                size: size.unwrap_or(sz_need as u16),
                value,
            })
        } else {
            Err("uint literal too large".to_string())
        }
    }
}

impl Number for Int {
    fn max_value(size: u16) -> BigInt {
        (BigInt::from(1) << size) - 1
    }

    fn min_value(size: u16) -> BigInt {
        -(BigInt::from(1) << size)
    }
}

impl Number for Uint {
    fn max_value(size: u16) -> BigInt {
        (BigInt::from(1) << size) - 1
    }

    fn min_value(_size: u16) -> BigInt {
        BigInt::from(0)
    }
}

pub fn number_literal_to_data(num: &Fraction) -> Result<Data, String> {
    if num.is_non_negative_integer() {
        Uint::new_from_literal(num.denominator.clone(), None)
            .map(|uint| Data::Uint(uint))
    } else if num.is_integer() {
        Int::new_from_literal(num.denominator.clone(), None)
            .map(|int| Data::Int(int))
    } else {
        Err("not an integer".to_string())
    }
}

// impl Operator for Int {
//     fn add(&mut self, rhs: &Data) -> Result<Data, String> {
//         self.op_assign(rhs, |a, b| a + b)
//     }


// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_op() {
        // let mut a = Data::Int(Int::new_from_literal());
        let a = BigInt::from(1000000000000000000 as i64);
        let b = BigInt::from(4);
        println!("{}", b.bits());
        println!("{}", a.clone()|b);
        let data = Data::Int(Int {
            size: 32,
            value: a,
        });

        println!("{}", data.to_string());
    }
}

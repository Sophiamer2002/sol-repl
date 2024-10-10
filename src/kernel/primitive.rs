use std::fmt::{Display, Formatter};
use crate::utils::fraction::Fraction;

use super::prelude::*;

use num_bigint::BigInt;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Bool(Bool),
    Num(Integer),
    Bytes(Bytes),
    Address(Address),
    NumLiteral(Fraction),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Bool(b) => write!(f, "{}", b.value),
            Primitive::Num(i) => write!(f, "{}", i.value),
            Primitive::Bytes(b) => write!(f, "{:?}", b.value),
            Primitive::Address(a) => write!(f, "{:?}", a.value),
            Primitive::NumLiteral(n) => write!(f, "{}", n),
        }
    }
}

impl Primitive {
    // check if the data is exactly of type ty.
    pub fn match_type_exact(&self, ty: &BasicType) -> bool {
        match self {
            Primitive::Bool(_) => matches!(ty, BasicType::Bool()),
            Primitive::Num(Integer { signed, size,.. }) => {
                match ty {
                    BasicType::Int(sz) => *signed && *size == *sz,
                    BasicType::Uint(sz) => !*signed && *size == *sz,
                    _ => false,
                }
            }
            Primitive::Bytes(Bytes{size, ..}) => matches!(ty, BasicType::Bytes(sz) if sz == size),
            Primitive::Address(Address { payable, .. }) => matches!(ty, BasicType::Address(p) if p == payable),
            Primitive::NumLiteral(_) => false,
        }
    }

    // check if self can be converted to ty implicitly.
    pub fn match_type_auto(&self, ty: &BasicType) -> bool {
        match self {
            Primitive::Bool(_) => matches!(ty, BasicType::Bool()),
            Primitive::Num(Integer { signed, size, .. }) => {
                match ty {
                    BasicType::Int(sz) => *signed && *sz <= *size,
                    BasicType::Uint(sz) => !*signed && *sz <= *size,
                    _ => false,
                }
            },
            Primitive::Bytes(Bytes{size, ..}) => matches!(ty, BasicType::Bytes(sz) if sz <= size),
            Primitive::Address(Address { payable, .. }) => matches!(ty, BasicType::Address(p) if !p || *payable),
            Primitive::NumLiteral(num) => {
                match ty {
                    BasicType::Int(sz) => num.is_integer() && bits_need(&num.numerator, true) <= *sz,
                    BasicType::Uint(sz) => num.is_non_negative_integer() && bits_need(&num.numerator, false) <= *sz,
                    _ => false,
                }
            },
        }
    }

    pub fn default(ty: &BasicType) -> Self {
        match ty {
            BasicType::Bool() => Primitive::Bool(Bool { value: false }),
            BasicType::Int(size) => Primitive::Num(Integer { signed: true, size: *size, value: BigInt::from(0) }),
            BasicType::Uint(size) => Primitive::Num(Integer { signed: false, size: *size, value: BigInt::from(0) }),
            BasicType::Bytes(size) => Primitive::Bytes(Bytes { size: *size, value: vec![0; *size as usize] }),
            BasicType::Address(true) => Primitive::Address(Address { payable: true, value: [0; 20] }),
            BasicType::Address(false) => Primitive::Address(Address { payable: false, value: [0; 20] }),
        }
    }

    // implements implicit conversion from 
    pub fn copy_from_data(&self, ty: &BasicType) -> Result<Self, String> {
        match (ty, self) {
            (BasicType::Bool(), Primitive::Bool(b)) => Ok(Primitive::Bool(b.clone())),
            (BasicType::Int(size), Primitive::Num(Integer { signed, size: sz, value })) => {
                if *signed && *sz <= *size {
                    Ok(Primitive::Num(Integer {
                        signed: true,
                        size: *size,
                        value: value.clone(),
                    }))
                } else {
                    Err(format!("cannot convert {:?} to int{}", self, size))
                }
            },
            (BasicType::Uint(size), Primitive::Num(Integer { signed, size: sz, value })) => {
                if !signed && *sz <= *size {
                    Ok(Primitive::Num(Integer {
                        signed: false,
                        size: *size,
                        value: value.clone(),
                    }))
                } else {
                    Err(format!("cannot convert {:?} to uint{}", self, size))
                }
            },
            (BasicType::Bytes(size), Primitive::Bytes(b)) => {
                if *size >= b.size {
                    let mut value = b.value.clone();
                    value.resize(*size as usize, *size);
                    return Ok(Primitive::Bytes(Bytes { size: *size, value }));
                }

                Err(format!("cannot convert {:?} to bytes{}", self, size))
            },
            (BasicType::Address(payable), Primitive::Address(a)) => {
                if a.payable || !*payable {
                    return Ok(Primitive::Address(Address { payable: *payable, value: a.value.clone() }));
                }

                Err(format!("cannot convert {:?} to address{}", self, payable))
            },
            (BasicType::Int(size), Primitive::NumLiteral(num)) => {
                if num.is_integer() {
                    let value = num.numerator.clone();
                    let sz = bits_need(&value, true);
                    if sz <= *size {
                        return Ok(Primitive::Num(Integer {
                            signed: true,
                            size: *size,
                            value,
                        }));
                    }
                }

                Err(format!("cannot convert {:?} to int{}", self, size))
            },
            (BasicType::Uint(size), Primitive::NumLiteral(num)) => {
                if num.is_non_negative_integer() {
                    let value = num.numerator.clone();
                    let sz = bits_need(&value, false);
                    if sz <= *size {
                        return Ok(Primitive::Num(Integer {
                            signed: false,
                            size: *size,
                            value,
                        }));
                    }
                }

                Err(format!("cannot convert {:?} to uint{}", self, size))
            },
            (_, _) => Err(format!("cannot convert {:?} to {:?}", self, ty)),
        } 
    }

    pub fn deduct_common_types(data: Vec<&Self>) -> Result<BasicType, String> {
        // TODO: both for vec<Self> and vec<&Self>
        if data.is_empty() {
            panic!("Cannot deduct common type from empty data");
        }

        if data.iter().all(|d| d.match_type_auto(&BasicType::Bool())) {
            return Ok(BasicType::Bool());
        }

        for i in (8..=256).step_by(8) {
            if data.iter().all(|d| d.match_type_auto(&BasicType::Int(i))) {
                return Ok(BasicType::Int(i));
            }

            if data.iter().all(|d| d.match_type_auto(&BasicType::Uint(i))) {
                return Ok(BasicType::Uint(i));
            }
        }

        for i in 1..=32 {
            if data.iter().all(|d| d.match_type_auto(&BasicType::Bytes(i))) {
                return Ok(BasicType::Bytes(i));
            }
        }

        if data.iter().all(|d| d.match_type_auto(&BasicType::Address(true))) {
            return Ok(BasicType::Address(true));
        }

        if data.iter().all(|d| d.match_type_auto(&BasicType::Address(false))) {
            return Ok(BasicType::Address(false));
        }

        Err("Cannot deduct common type".to_string())
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
pub struct Integer {
    pub signed: bool,
    pub size: u16,
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
    fn bitwise_and(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn bitwise_not(&mut self) -> Result<Primitive, String>;

    fn bitwise_or(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn bitwise_xor(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn add(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn divide(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn multiply(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn modulo(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn subtract(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn shift_left(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn shift_right(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn equal(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn more(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn more_or_equal(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn less(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn less_or_equal(&mut self, rhs: &Primitive) -> Result<Primitive, String>;

    fn not_equal(&mut self, rhs: &Primitive) -> Result<Primitive, String>;
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

    // pub fn op_assign(&mut self, rhs: &Data, op: fn(&BigInt, &BigInt) -> BigInt) -> Result<Data, String> {
    //     if let Data::Int(rhs) = rhs {
    //         let sz = self.size.max(rhs.size);
    //         let value = op(&self.value, &rhs.value);

    //         if value > Self::max_value(sz) || value < Self::min_value(sz) {
    //             return Err("overflow".to_string());
    //         }

    //         return Ok(Data::Int(Int {
    //             size: sz,
    //             value,
    //         }));
    //     }

    //     Err("type not match".to_string())
    // }
}

impl Uint {
    pub fn new_from_literal(value: BigInt, size: Option<u16>) -> Result<Self, String> {
        let sz_need = (value.bits() + 7) & (!7);
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

pub fn bits_need(num: &BigInt, signed: bool) -> u16 {
    // TODO: num might be too big
    //       or not consistent with the signed flag.
    let mut bits = num.bits();
    if num < &BigInt::from(0) && signed {
        bits = (-num.clone() - BigInt::from(1)).bits() + 1;
    }

    ((bits + 7) & (!7)) as u16
}

// impl Operator for Int {
//     fn add(&mut self, rhs: &Data) -> Result<Data, String> {
//         self.op_assign(rhs, |a, b| a + b)
//     }


// }

#[cfg(test)]
mod tests {
    use super::*;
    use std::{rc::Rc, cell::RefCell, collections::HashMap};

    #[test]
    fn test_int_op() {
        // let mut a = Data::Int(Int::new_from_literal());
        let a = BigInt::from(1000000000000000000 as i64);
        let b = BigInt::from(4);
        println!("{}", b.bits());
        println!("{}", a.clone()|b);
        let data = Primitive::NumLiteral( Fraction::one() );

        println!("{}", data.to_string());
    }

    #[test]
    fn test_big_int_sz() {
        let v: Vec<_> = [0, 1, -1, 2, -2, 3, -3, 4, -4, 7, -7, 8, -8, 15, -15, 16, -16]
            .iter()
            .map(|x| BigInt::from(*x).bits())
            .collect();

        println!("{:?}", v);
    }

    #[test]
    fn test_rc_mutable() {
        // let a = Rc::new(Box::new(1));
        let a = Box::new(1);
        let mut a = a;
        a = Box::new(2);
        *a += 10;

        let b = Rc::new(RefCell::new(HashMap::new()));
        (*b).borrow_mut().insert("a", 1);
        let bb = Rc::new((&*b).clone());
        (*bb).borrow_mut().insert("b", 2);

        for _ in 7..8 {
            println!("a");
        }
        dbg!(&*b);
        dbg!(&*bb);
    }
}

use std::{collections::HashMap, fmt::{Display, Formatter}, rc::Rc};
use super::prelude::*;

use num_bigint::BigInt;

use crate::utils::fraction::Fraction;

#[derive(Clone, Debug)]
pub enum Data {
    Void(),
    NumberLiteral(Fraction),
    Bool(Bool),
    Int(Int),
    Uint(Uint),
    Bytes(Bytes),
    Address(Address),
    StringQ(StringQ),

    Array(Array),
    Struct(Struct),
    Mapping(Mapping),
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Void() => write!(f, "void"),
            Data::NumberLiteral(n) => write!(f, "{}", n),
            Data::Bool(b) => write!(f, "{}", b.value),
            Data::Int(i) => write!(f, "{}", i.value),
            Data::Uint(u) => write!(f, "{}", u.value),
            Data::Bytes(b) => write!(f, "{:?}", b.value),
            Data::Address(a) => write!(f, "{:?}", a.value),
            Data::StringQ(s) => write!(f, "{}", s.value),
            Data::Array(a) => write!(f, "{:?}", a.values),
            Data::Struct(s) => write!(f, "{:?}", s.fields),
            Data::Mapping(m) => write!(f, "{:?}", m.map),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub table: HashMap<String, Entry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            table: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Entry {
    Type(Rc<Type>),
    Data(Data),
}

#[derive(Clone, Debug)]
pub struct Int {
    pub size: u8, // in bytes.
    pub value: BigInt,
}

#[derive(Clone, Debug)]
pub struct Uint {
    pub size: u8, // in bytes.
    pub value: BigInt,
}

#[derive(Clone, Debug)]
pub struct Bool {
    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct Bytes {
    pub size: Option<u8>, // in bytes. if none, then dynamic
    pub value: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct Address {
    pub payable: bool,
    pub value: [u8; 20],
}

#[derive(Clone, Debug)]
pub struct StringQ {
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub ty: Rc<Type>,
    pub fields: Vec<Data>,
}

#[derive(Clone, Debug)]
pub struct Mapping {
    pub ty: Rc<Type>,
    pub map: HashMap<Data, Data>,
}

#[derive(Clone, Debug)]
pub struct Array {
    pub ty: Rc<Type>,
    pub values: Vec<Data>,
}

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
    fn max_value(size: u8) -> BigInt;
    fn min_value(size: u8) -> BigInt;
}

impl Int {
    pub fn new_from_literal(value: BigInt) -> Self {
        let size = value.bits();
        if size > 256 {
            panic!("int literal too large");
        }
        Int {
            size: size as u8,
            value,
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

impl Number for Int {
    fn max_value(size: u8) -> BigInt {
        (BigInt::from(1) << (size * 8)) - 1
    }

    fn min_value(size: u8) -> BigInt {
        -(BigInt::from(1) << (size * 8))
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
        let b = BigInt::from(-1);
        println!("{}", a.clone()|b);
        let data = Data::Int(Int {
            size: 32,
            value: a,
        });

        println!("{}", data.to_string());
    }
}

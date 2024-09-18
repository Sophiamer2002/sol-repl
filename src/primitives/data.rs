use std::{collections::HashMap, fmt::{Display, Formatter}, rc::Rc};
use super::prelude::*;

use num_bigint::BigInt;

use crate::utils::fraction::Fraction;

#[derive(Clone, Debug)]
pub enum Data {
    Void(),
    NumberLiteral(Fraction),
    Basic(BasicData),

    Array(Array),
    Struct(Struct),
    Mapping(Mapping),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BasicData {
    Bool(Bool),
    Int(Int),
    Uint(Uint),
    Bytes(Bytes),
    Address(Address),
    StringQ(StringQ),
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Void() => write!(f, "void"),
            Data::NumberLiteral(n) => write!(f, "{}", n),
            Data::Basic(b) => write!(f, "{}", b),
            Data::Array(a) => write!(f, "{:?}", a),
            Data::Struct(s) => write!(f, "{:?}", s),
            Data::Mapping(m) => write!(f, "{:?}", m),
        }
    }
}

impl Display for BasicData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicData::Bool(b) => write!(f, "{}", b.value),
            BasicData::Int(i) => write!(f, "{}", i.value),
            BasicData::Uint(u) => write!(f, "{}", u.value),
            BasicData::Bytes(b) => write!(f, "{:?}", b.value),
            BasicData::Address(a) => write!(f, "{:?}", a.value),
            BasicData::StringQ(s) => write!(f, "{}", s.value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    table: HashMap<String, Entry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Entry> {
        self.table.get(name)
    }

    pub fn insert_variable(&mut self, name: String, data: Data) -> Result<(), String> {
        if self.table.contains_key(&name) {
            return Err(format!("variable {} already exists in the symbol table", name));
        }
        self.table.insert(name.clone(), Entry::Data(data));
        Ok(())
    }

    pub fn insert_type(&mut self, name: String, ty: Rc<Type>) -> Result<(), String> {
        if self.table.contains_key(&name) {
            return Err(format!("variable {} already exists in the symbol table", name));
        }

        self.table.insert(name, Entry::Type(ty));
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum Entry {
    Type(Rc<Type>),
    Data(Data),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Int {
    pub size: u8, // in bytes.
    pub value: BigInt,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Uint {
    pub size: u8, // in bytes.
    pub value: BigInt,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bool {
    pub value: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bytes {
    pub size: Option<u8>, // in bytes. if none, then dynamic
    pub value: Vec<u8>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Address {
    pub payable: bool,
    pub value: [u8; 20],
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
    pub map: HashMap<BasicData, Data>,
}

#[derive(Clone, Debug)]
pub struct Array {
    pub ty: Rc<Type>,
    pub values: HashMap<u32, Data>,
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

pub trait DefaultValue {
    fn default_value(size: Option<u8>) -> Self;
}

impl Int {
    pub fn new_from_literal(value: BigInt, size: Option<u8>) -> Result<Self, String> {
        let vv = if value >= BigInt::from(0) {
            value.clone()
        } else {
            -value.clone() - 1
        };
        let sz_need = (vv.bits() + 7) / 8 + 1;
        if sz_need <= size.unwrap_or(32) as u64 {
            Ok(Int {
                size: size.unwrap_or(sz_need as u8),
                value,
            })
        } else {
            Err("int literal too large".to_string())
        }
    }

    pub fn op_assign(&mut self, rhs: &Data, op: fn(&BigInt, &BigInt) -> BigInt) -> Result<Data, String> {
        if let Data::Basic(BasicData::Int(rhs)) = rhs {
            let sz = self.size.max(rhs.size);
            let value = op(&self.value, &rhs.value);

            if value > Self::max_value(sz) || value < Self::min_value(sz) {
                return Err("overflow".to_string());
            }

            return Ok(Data::Basic(BasicData::Int(Int {
                size: sz,
                value,
            })));
        }

        Err("type not match".to_string())
    }
}

impl Uint {
    pub fn new_from_literal(value: BigInt, size: Option<u8>) -> Result<Self, String> {
        let sz_need = (value.bits() + 7) / 8;
        if sz_need <= size.unwrap_or(32) as u64 {
            Ok(Uint {
                size: size.unwrap_or(sz_need as u8),
                value,
            })
        } else {
            Err("uint literal too large".to_string())
        }
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

impl Number for Uint {
    fn max_value(size: u8) -> BigInt {
        (BigInt::from(1) << (size * 8)) - 1
    }

    fn min_value(size: u8) -> BigInt {
        BigInt::from(0)
    }
}

impl DefaultValue for Bool {
    fn default_value(_: Option<u8>) -> Self {
        Bool { value: false }
    }
}

impl DefaultValue for Int {
    fn default_value(size: Option<u8>) -> Self {
        let size = size.unwrap_or(32);
        Int {
            size,
            value: BigInt::from(0),
        }
    }
}

impl DefaultValue for Uint {
    fn default_value(size: Option<u8>) -> Self {
        let size = size.unwrap_or(32);
        Uint {
            size,
            value: BigInt::from(0),
        }
    }
}

impl DefaultValue for Bytes {
    fn default_value(size: Option<u8>) -> Self {
        Bytes {
            size,
            value: vec![0; size.unwrap_or(0) as usize],
        }
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
        let data = Data::Basic(BasicData::Int(Int {
            size: 32,
            value: a,
        }));

        println!("{}", data.to_string());
    }
}

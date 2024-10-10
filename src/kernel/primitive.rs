use std::fmt::{Display, Formatter};

use super::prelude::*;

use num_bigint::BigInt;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Bool(bool),
    Num(BigInt),
    Bytes([u8; 32]),
    Address([u8; 20]),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Bool(b) => write!(f, "{}", b),
            Primitive::Num(i) => write!(f, "{}", i),
            Primitive::Bytes(b) => write!(f, "{:?}", b),
            Primitive::Address(a) => write!(f, "{:?}", a),
        }
    }
}

impl Primitive {
    pub fn default(ty: &BasicType) -> Self {
        match ty {
            BasicType::Bool() => Primitive::Bool(false),
            BasicType::Int(_) => Primitive::Num(BigInt::from(0)),
            BasicType::Uint(_) => Primitive::Num(BigInt::from(0)),
            BasicType::Bytes(_) => Primitive::Bytes([0; 32]),
            BasicType::Address(_) => Primitive::Address([0; 20]),
        }
    }

    pub fn from_integer(i: u32) -> Self {
        Primitive::Num(BigInt::from(i))
    }

    pub fn sanity_check(&self, ty: &BasicType) {
        match (self, ty) {
            (Primitive::Bool(_), BasicType::Bool()) => {}
            (Primitive::Num(i), BasicType::Int(sz)) => {
                assert!(bits_need(&i, true) <= *sz);
            }
            (Primitive::Num(i), BasicType::Uint(sz)) => {
                assert!(*i >= BigInt::ZERO);
                assert!(bits_need(&i, false) <= *sz);
            }
            (Primitive::Bytes(b), BasicType::Bytes(sz)) => {
                for i in (*sz as usize)..32 {
                    assert_eq!(b[i], 0);
                }
            }
            (Primitive::Address(_), BasicType::Address(_)) => {},
            _ => panic!("type not match"),
        }
    }

    pub fn overflow(&self, ty: &BasicType) -> bool {
        match (self, ty) {
            (Primitive::Num(i), BasicType::Int(sz)) => {
                bits_need(&i, true) > *sz
            }
            (Primitive::Num(i), BasicType::Uint(sz)) => {
                *i < BigInt::ZERO || bits_need(&i, false) > *sz
            }
            _ => unreachable!(),
        }
    }
}

/// see https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.userDefinableOperator
pub trait Operators {
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
        // let data = Primitive::NumLiteral( Fraction::one() );

        // println!("{}", data.to_string());
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

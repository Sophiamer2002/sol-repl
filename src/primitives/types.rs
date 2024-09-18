use std::{collections::HashSet, rc::Rc};
use crate::utils::fraction::Fraction;

use super::prelude::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Number(),
    Basic(BasicType),
    Struct {
        identifier: String,
        fields: Vec<(String, Rc<Type>)>
    },
    Mapping {
        key: Rc<Type>, 
        value: Rc<Type>
    },
    Array {
        len: Option<u32>,
        value: Rc<Type>
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BasicType {
    Bool(),
    Int(u16),
    Uint(u16),
    Bytes(Option<u8>), // None means dynamic
    Address(bool), // payable or not
    String(),
}

impl BasicType {
    pub fn default_data(&self) -> Data {
        match self {
            BasicType::Bool() => Data::Basic(BasicData::Bool(Bool { value: false })),
            BasicType::Int(size) => Data::Basic(BasicData::Int(Int::default_value(Some((size / 8) as u8)))),
            BasicType::Uint(size) => Data::Basic(BasicData::Uint(Uint::default_value(Some((size / 8) as u8)))),
            BasicType::Bytes(size) => Data::Basic(BasicData::Bytes(Bytes::default_value(*size))),
            BasicType::Address(true) => Data::Basic(BasicData::Address(Address{ payable: true, value: [0; 20] })),
            BasicType::Address(false) => Data::Basic(BasicData::Address(Address{ payable: false, value: [0; 20] })),
            BasicType::String() => Data::Basic(BasicData::StringQ(StringQ { value: String::new() })),
        }
    }
}

pub fn implicit_conversion(ty: &Rc<Type>, data: &Data) -> Result<Data, String> {
    match (&**ty, data) {
        (Type::Basic(BasicType::Int(sz)), Data::NumberLiteral(num)) => {
            if num.is_integer() {
                let num = num.denominator.clone();
                Int::new_from_literal(num, Some((sz / 8) as u8))
                    .map(|int| Data::Basic(BasicData::Int(int)))
            } else {
                Err(format!("cannot convert a non-integer number {} to int{}", num, sz))
            }
        },


        (Type::Basic(BasicType::Int(sz)), Data::Basic(BasicData::Int(i))) => {
            if *sz >= (i.size as u16) * 8 {
                Int::new_from_literal(i.value.clone(), Some((sz / 8) as u8))
                    .map(|int| Data::Basic(BasicData::Int(int)))
            } else {
                Err(format!("cannot convert int{} to int{}", i.size * 8, sz))
            }
        },

        (Type::Basic(BasicType::Uint(sz)), Data::NumberLiteral(num)) => {
            if num.is_integer() {
                let num = num.denominator.clone();
                Uint::new_from_literal(num, Some((sz / 8) as u8))
                    .map(|uint| Data::Basic(BasicData::Uint(uint)))
            } else {
                Err(format!("cannot convert a non-integer number {} to uint{}", num, sz))
            }
        },

        (Type::Basic(BasicType::Uint(sz)), Data::Basic(BasicData::Uint(i))) => {
            if *sz >= (i.size as u16) * 8 {
                Uint::new_from_literal(i.value.clone(), Some((sz / 8) as u8))
                    .map(|uint| Data::Basic(BasicData::Uint(uint)))
            } else {
                Err(format!("cannot convert uint{} to uint{}", i.size * 8, sz))
            }
        },
        _ => Err(format!("cannot convert {:?} to {:?}", data, ty))
    }
}

pub fn get_default_data(ty: &Rc<Type>) -> Data {
    match &**ty {
        Type::Number() => Data::NumberLiteral(Fraction::zero()),
        Type::Basic(basic) => basic.default_data(),
        Type::Struct { identifier: _, fields } => {
            let fields = fields.iter().map(|(_, ty)| get_default_data(ty)).collect();
            Data::Struct(Struct { ty: ty.clone(), fields })
        },
        Type::Mapping { key: _, value: _ } => Data::Mapping(Mapping { ty: ty.clone(), map: Default::default() }),
        Type::Array { len: _, value: _ } => Data::Array(Array { ty: ty.clone(), values: Default::default() }),
    }
}

#[derive(Clone, Debug)]
pub struct TypeTable {
    table: HashSet<Rc<Type>>,
}

impl TypeTable {
    pub fn new() -> Self {
        let mut table = HashSet::new();
        table.insert(Rc::new(Type::Number()));
        table.insert(Rc::new(Type::Basic(BasicType::Bool())));

        for i in 8..=256 {
            table.insert(Rc::new(Type::Basic(BasicType::Int(i))));
            table.insert(Rc::new(Type::Basic(BasicType::Uint(i))));
        }

        table.insert(Rc::new(Type::Basic(BasicType::Bytes(None))));
        for i in 1..=32 {
            table.insert(Rc::new(Type::Basic(BasicType::Bytes(Some(i)))));
        }

        table.insert(Rc::new(Type::Basic(BasicType::Address(false))));
        table.insert(Rc::new(Type::Basic(BasicType::Address(true))));

        table.insert(Rc::new(Type::Basic(BasicType::String())));
        Self {
            table
        }
    }

    pub fn get_type(&self, ty: &Type) -> Option<Rc<Type>> {
        self.table.get(ty).cloned()
    }

    pub fn get_or_insert(&mut self, ty: Type) -> Rc<Type> {
        if let Some(t) = self.table.get(&ty) {
            t.clone()
        } else {
            let t = Rc::new(ty.clone());
            self.table.insert(t.clone());
            t
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type() {
        let mut table = TypeTable {
            table: HashSet::new(),
        };

        table.table.insert(Rc::new(Type::Basic(BasicType::Int(256))));
        {
            let ty = Rc::new(Type::Basic(BasicType::Int(256)));
            println!("{}", table.table.contains(&ty));
        }

        let ty = Rc::new(Type::Basic(BasicType::Int(256)));
        let _ = table.table.get(&ty).unwrap().clone();
    }
}
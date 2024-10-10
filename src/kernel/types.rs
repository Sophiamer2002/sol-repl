use std::rc::Rc;
use super::prelude::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    B(BasicType),
    C(CompoundType),
    Literal,
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
        fields: Vec<(String, Rc<Type>)>
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

// impl BasicType {
//     // check if data can be implicitly converted to self(basic type)
//     // and return the converted data if possible.
//     pub fn fit(&self, mut data: Data) -> Result<Data, String> {
//         match self {
//             BasicType::Bool() => match data {
//                 Data::Bool(_) => Ok(data),
//                 _ => Err(format!("cannot convert {:?} to bool", data))
//             },
//             BasicType::Int(size) => match &mut data {
//                 Data::Int(i) => {
//                     if i.size <= *size {
//                         i.size = *size;
//                         Ok(data)
//                     } else {
//                         Err(format!("cannot convert int{} to int{}", i.size, size))
//                     }
//                 },
//                 _ => Err(format!("cannot convert {:?} to int{}", data, size))
//             },
//             BasicType::Uint(size) => match &mut data {
//                 Data::Uint(i) => {
//                     if i.size <= *size {
//                         i.size = *size;
//                         Ok(data)
//                     } else {
//                         Err(format!("cannot convert uint{} to uint{}", i.size, size))
//                     }
//                 },
//                 _ => Err(format!("cannot convert {:?} to uint{}", data, size))
//             },
//             BasicType::Bytes(size) => match &mut data {
//                 Data::Bytes(b) => {
//                     if b.size <= *size {
//                         b.size = *size;
//                         b.value.resize(*size as usize, 0);
//                         Ok(data)
//                     } else {
//                         Err(format!("cannot convert bytes{} to bytes{}", b.size, size))
//                     }
//                 },
//                 _ => Err(format!("cannot convert {:?} to bytes{}", data, size))
//             },
//             BasicType::Address(payable) => match &mut data {
//                 Data::Address(a) => {
//                     if a.payable <= *payable {
//                         a.payable = *payable;
//                         Ok(data)
//                     } else {
//                         Err(format!("cannot convert address{} to address{}", a.payable, payable))
//                     }
//                 },
//                 _ => Err(format!("cannot convert {:?} to address{}", data, payable))
//             },
//         }
//     }
// }

impl Type {
    // check if data of the type can be stored in memory
    pub fn memory_friendly(&self) -> bool {
        match self {
            Type::B(_) => true,
            Type::C(c) => match c {
                CompoundType::String => true,
                CompoundType::DynamicBytes => true,
                CompoundType::Struct { .. } => true,
                CompoundType::Mapping { .. } => false,
                CompoundType::Array { .. } => true,
            },
            Type::Literal => false,
        }
    }
}

// impl BasicType {
//     pub fn default_data(&self) -> Data {
//         match self {
//             BasicType::Bool() => Data::Basic(BasicData::Bool(Bool { value: false })),
//             BasicType::Int(size) => Data::Basic(BasicData::Int(Int::default_value(Some((size / 8) as u8)))),
//             BasicType::Uint(size) => Data::Basic(BasicData::Uint(Uint::default_value(Some((size / 8) as u8)))),
//             BasicType::Bytes(size) => Data::Basic(BasicData::Bytes(Bytes::default_value(*size))),
//             BasicType::Address(true) => Data::Basic(BasicData::Address(Address{ payable: true, value: [0; 20] })),
//             BasicType::Address(false) => Data::Basic(BasicData::Address(Address{ payable: false, value: [0; 20] })),
//             BasicType::String() => Data::Basic(BasicData::StringQ(StringQ { value: String::new() })),
//         }
//     }
// }

// pub fn implicit_conversion(ty: &Rc<Type>, data: &Data) -> Result<Data, String> {
//     match (&**ty, data) {
//         (Type::Basic(BasicType::Int(sz)), Data::NumberLiteral(num)) => {
//             if num.is_integer() {
//                 let num = num.denominator.clone();
//                 Int::new_from_literal(num, Some((sz / 8) as u8))
//                     .map(|int| Data::Basic(BasicData::Int(int)))
//             } else {
//                 Err(format!("cannot convert a non-integer number {} to int{}", num, sz))
//             }
//         },


//         (Type::Basic(BasicType::Int(sz)), Data::Basic(BasicData::Int(i))) => {
//             if *sz >= (i.size as u16) * 8 {
//                 Int::new_from_literal(i.value.clone(), Some((sz / 8) as u8))
//                     .map(|int| Data::Basic(BasicData::Int(int)))
//             } else {
//                 Err(format!("cannot convert int{} to int{}", i.size * 8, sz))
//             }
//         },

//         (Type::Basic(BasicType::Uint(sz)), Data::NumberLiteral(num)) => {
//             if num.is_integer() {
//                 let num = num.denominator.clone();
//                 Uint::new_from_literal(num, Some((sz / 8) as u8))
//                     .map(|uint| Data::Basic(BasicData::Uint(uint)))
//             } else {
//                 Err(format!("cannot convert a non-integer number {} to uint{}", num, sz))
//             }
//         },

//         (Type::Basic(BasicType::Uint(sz)), Data::Basic(BasicData::Uint(i))) => {
//             if *sz >= (i.size as u16) * 8 {
//                 Uint::new_from_literal(i.value.clone(), Some((sz / 8) as u8))
//                     .map(|uint| Data::Basic(BasicData::Uint(uint)))
//             } else {
//                 Err(format!("cannot convert uint{} to uint{}", i.size * 8, sz))
//             }
//         },
//         _ => Err(format!("cannot convert {:?} to {:?}", data, ty))
//     }
// }

// pub fn get_default_data(ty: &Rc<Type>) -> Data {
//     match &**ty {
//         Type::Basic(basic) => basic.default_data(),
//         Type::Struct { identifier: _, fields } => {
//             let fields = fields.iter().map(|(_, ty)| get_default_data(ty)).collect();
//             Data::Struct(Struct { ty: ty.clone(), fields })
//         },
//         Type::Mapping { key: _, value: _ } => Data::Mapping(Mapping { ty: ty.clone(), map: Default::default() }),
//         Type::Array { len: _, value: _ } => Data::Array(Array { ty: ty.clone(), values: Default::default() }),
//     }
// }
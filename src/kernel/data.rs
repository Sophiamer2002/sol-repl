use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::utils::fraction::Fraction;

use super::prelude::*;

pub type Ty = Rc<Type>;

#[derive(Clone, Debug)]
pub struct DataRef {
    pub ty: Ty,
    pub data: Rc<RefCell<Data>>,
}

#[derive(Clone, Debug)]
pub enum Data {
    Struct(HashMap<String, DataRef>),
    Array(Vec<DataRef>),
    Mapping(HashMap<Primitive, DataRef>),
    Basic(Primitive),
}

impl DataRef {
    pub fn default(ty: Ty) -> Self {
        let data = match &*ty {
            Type::B(ty_) => Self {
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::default(ty_)))),
            },
            Type::C(CompoundType::Array { len, value }) => Self {
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Array(
                    (0..(*len).unwrap_or(0)).map(|_| DataRef::default(value.clone())).collect(),
                ))),
            },
            Type::C(CompoundType::Mapping { .. }) => Self {
                ty,
                data: Rc::new(RefCell::new(Data::Mapping(HashMap::new()))),
            },
            Type::C(CompoundType::Struct { fields, .. }) => Self {
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Struct(
                    fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), DataRef::default(ty.clone())))
                        .collect(),
                ))),
            },
            Type::C(CompoundType::DynamicBytes) => unimplemented!(),
            Type::C(CompoundType::String) => unimplemented!(),
            Type::Literal => panic!("Literal type cannot be default"),
        };
        data.sanity_check();
        data
    }

    pub fn copy_from_data(&self, ty: Ty, is_lvalue: bool) -> Result<Self, String> {
        // if it is an rvalue, then we can directly return the
        // data if the type matches exactly
        self.sanity_check();
        if !is_lvalue {
            if self.ty == ty {
                return Ok(self.clone());
            }
        }

        let data = match (&*self.ty, &*ty) {
            (Type::B(b_ty), Type::B(self_b_ty)) => {
                match &*(self.data).borrow() {
                    Data::Basic(b_data) => b_data.copy_from_data(&b_ty).map(|data| Self {
                        ty,
                        data: Rc::new(RefCell::new(Data::Basic(data))),
                    }),
                    _ => unreachable!(),
                }
            },

            (Type::C(CompoundType::Array { len, value: _value }), Type::C(CompoundType::Array { len: new_len, value: new_value })) => {
                if len.unwrap_or(!0) <= new_len.unwrap_or(!0) {
                    match &*(self.data).borrow() {
                        Data::Array(arr) => {
                            let mut new_arr: Vec<_> = arr
                                .iter()
                                .map(|data| data.copy_from_data(new_value.clone(), is_lvalue))
                                .collect::<Result<_, _>>()?;
                            let len = arr.len();
                            new_arr.extend(
                                (len..new_len.unwrap_or(0) as usize)
                                    .map(|_| DataRef::default(new_value.clone())));
                            Ok(Self {
                                ty,
                                data: Rc::new(RefCell::new(Data::Array(new_arr))),
                            })
                        },
                        _ => unreachable!(),
                    }
                } else {
                    Err("Cannot copy from a larger array".to_string())
                }
            },

            (Type::C(CompoundType::Mapping { .. }), _) => {
                Err("Cannot copy a mapping".to_string())
            },

            (Type::C(CompoundType::Struct { .. }), Type::C(CompoundType::Struct { .. })) => {
                if self.ty != ty {
                    return Err("Type mismatch".to_string());
                }
                let new_data = (&*(self.data)).clone();
                Ok(Self { ty, data: Rc::new(new_data) })
            },

            (Type::C(CompoundType::DynamicBytes), _) => unimplemented!(),
            (Type::C(CompoundType::String), _) => unimplemented!(),
            (Type::Literal, _) => Err("Type mismatch".to_string()),
            (_, _) => Err("Type mismatch or unimplemented".to_string()),
        }?;
        data.sanity_check();
        Ok(data)
    }

    pub fn new_array(ty: Ty, data: Vec<Self>) -> Self {
        let data = Self {
            ty,
            data: Rc::new(RefCell::new(Data::Array(data))),
        };
        data.sanity_check();
        data
    }

    pub fn number_literal_to_data(num: String) -> Result<Self, String> {
        let data = if num.starts_with("0x") {
            // case 1: num starts with "0x", it is a hex number
            let num = num.trim_start_matches("0x").replace("_", "");
            let num = Fraction::new_from_hex_num_literal(num)?;
            Self {
                ty: Rc::new(Type::Literal),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::NumLiteral(num)))),
            }
        } else {
            // case 2: num is a decimal number
            let num = Fraction::new_from_number_literal(num)?;
            Self {
                ty: Rc::new(Type::Literal),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::NumLiteral(num)))),
            }
        };

        data.sanity_check();
        Ok(data)
    }

    pub fn extract_number_literal(&self) -> Option<Fraction> {
        match &*(self.data).borrow() {
            Data::Basic(Primitive::NumLiteral(num)) => Some(num.clone()),
            _ => None,
        }
    }

    /// builtin members:
    ///    - array.length
    ///
    ///    - struct.fields
    pub fn extract_member(&self, name: String) -> Result<(Self, bool), String> {
        unimplemented!()
    }

    /// builtin functions:
    ///     - array.push( [data] )
    ///     - array.pop()
    pub fn call_method(&self, name: String, args: Vec<Self>) -> Result<Self, String> {
        unimplemented!()
    }

    // TODO: can create a macro for vanity_check
    fn sanity_check(&self) {
        match &*self.ty {
            Type::B(_) => {
                // TODO
                return
            },
            Type::C(CompoundType::Array { len, value }) => {
                match &*(self.data).borrow() {
                    Data::Array(arr) => {
                        if let Some(len) = len {
                            if arr.len() != *len as usize {
                                panic!("Array length mismatch");
                            }
                        }

                        for data in arr {
                            if data.ty != *value {
                                panic!("Array element type mismatch");
                            }
                            data.sanity_check();
                        }
                    },
                    _ => panic!("Array data is not an array"),
                }
            },
            Type::C(CompoundType::Mapping { key, value }) => {
                match &*(self.data).borrow() {
                    Data::Mapping(map) => {
                        for (key_data, data) in map {
                            if data.ty != *value {
                                panic!("Mapping value type mismatch");
                            }
                            if let Type::B(key_ty) = &**key {
                                if !key_data.match_type_exact(key_ty) {
                                    panic!("Mapping key type mismatch");
                                }
                            } else {
                                panic!("Mapping key type is not a basic type");
                            }
                            data.sanity_check();
                        }
                    },
                    _ => panic!("Mapping data is not a mapping"),
                }
            },
            Type::C(CompoundType::Struct { fields, .. }) => {
                match &*(self.data).borrow() {
                    Data::Struct(map) => {
                        for (name, ty) in fields {
                            if let Some(data) = map.get(name) {
                                if data.ty != *ty {
                                    panic!("Struct field type mismatch");
                                }
                                data.sanity_check();
                            } else {
                                panic!("Struct field not found");
                            }
                        }
                    },
                    _ => unreachable!(),
                }
            },
            Type::C(CompoundType::DynamicBytes) => unimplemented!(),
            Type::C(CompoundType::String) => unimplemented!(),
            Type::Literal => {
                match &*(self.data).borrow() {
                    Data::Basic(Primitive::NumLiteral(_)) => {},
                    _ => panic!("Data is not a literal"),
                }
            }
        }
    }

    pub fn deduct_common_types(data: Vec<&Self>) -> Result<Ty, String> {
        if data.is_empty() {
            panic!("Cannot deduct common type from empty data");
        }

        if data.iter().all(|d| matches!(&*(d.data).borrow(), Data::Basic(_))) {
            let data: Vec<_> = data.iter().map(|d| {
                match &*(d.data).borrow() {
                    Data::Basic(b) => b.clone(),
                    _ => unreachable!(),
                }
            }).collect();

            let data = data.iter().map(|d| d).collect::<Vec<_>>();

            let ty = Primitive::deduct_common_types(data)?;
            return Ok(Rc::new(Type::B(ty)));
        }

        // if compound data exists, then all data must be the same type
        let tys = data.iter().map(|d| d.ty.clone()).collect::<Vec<_>>();
        if tys.iter().all(|ty| ty == &tys[0]) {
            Ok(tys[0].clone())
        } else {
            Err("Cannot deduct common type".to_string())
        }
    }

}

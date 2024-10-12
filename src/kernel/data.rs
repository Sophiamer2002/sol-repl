use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::utils::fraction::Fraction;

use super::prelude::*;

pub type DataCell = Rc<RefCell<Data>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Indirection {
    LValue,
    RValue,
    Ref,
}

#[derive(Clone, Debug)]
pub struct DataRef {
    pub indirection: Indirection,
    pub ty: Ty,
    pub data: DataCell,
}

#[derive(Clone, Debug)]
pub enum Data {
    Struct(HashMap<String, DataCell>),
    Array(Vec<DataCell>),
    Mapping(HashMap<Primitive, DataCell>),
    Basic(Primitive),
    // TODO
    // Reference(DataCell),
    Literal,
}

/// see https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.userDefinableOperator
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    // bit operations
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,

    // comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    More,
    MoreEqual,

    // logical
    And,
    Or,
}

impl Data {
    pub fn deep_copy(&self) -> Self {
        match self {
            Data::Struct(map) => Data::Struct(
                map.iter()
                    .map(|(name, data)| {
                        (
                            name.clone(),
                            Rc::new(RefCell::new(data.borrow().deep_copy())),
                        )
                    })
                    .collect(),
            ),
            Data::Array(arr) => Data::Array(
                arr.iter()
                    .map(|data| Rc::new(RefCell::new(data.borrow().deep_copy())))
                    .collect(),
            ),
            Data::Mapping(map) => Data::Mapping(
                map.iter()
                    .map(|(key, data)| {
                        (
                            key.clone(),
                            Rc::new(RefCell::new(data.borrow().deep_copy())),
                        )
                    })
                    .collect(),
            ),
            Data::Basic(b) => Data::Basic(b.clone()),
            Data::Literal => Data::Literal,
        }
    }

    pub fn sanity_check(&self, ty: Ty) {
        match (self, &*ty) {
            (Data::Basic(b), Type::B(ty_)) => {
                b.sanity_check(ty_);
            }
            (Data::Array(arr), Type::C(CompoundType::Array { len, value })) => {
                if let Some(len) = len {
                    assert_eq!(arr.len(), *len as usize);
                }

                for data in arr.iter() {
                    data.borrow().sanity_check(value.clone());
                }
            }
            (Data::Mapping(map), Type::C(CompoundType::Mapping { key, value })) => {
                for (k, v) in map.iter() {
                    Data::Basic(k.clone()).sanity_check(key.clone());
                    v.borrow().sanity_check(value.clone());
                }
            }
            (Data::Struct(map), Type::C(CompoundType::Struct { fields, .. })) => {
                for (name, ty) in fields.iter() {
                    let data = map.get(name).unwrap();
                    data.borrow().sanity_check(ty.clone());
                }
            }
            (Data::Literal, Type::NumLiteral(_)) => {}
            _ => unreachable!(),
        }
    }
}

impl DataRef {
    // TODO: the main part of default should be moved to Data struct
    pub fn default(ty: Ty, indirection: Indirection) -> Self {
        assert!(indirection != Indirection::Ref);
        let data = match &*ty {
            Type::B(ty_) => Self {
                indirection,
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::default(ty_)))),
            },
            Type::C(CompoundType::Array { len, value }) => Self {
                indirection,
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Array(
                    (0..(*len).unwrap_or(0))
                        .map(|_| DataRef::default(value.clone(), indirection).data)
                        .collect(),
                ))),
            },
            Type::C(CompoundType::Mapping { .. }) => Self {
                indirection,
                ty,
                data: Rc::new(RefCell::new(Data::Mapping(HashMap::new()))),
            },
            Type::C(CompoundType::Struct { fields, .. }) => Self {
                indirection,
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Struct(
                    fields
                        .iter()
                        .map(|(name, ty)| {
                            (name.clone(), DataRef::default(ty.clone(), indirection).data)
                        })
                        .collect(),
                ))),
            },
            Type::C(CompoundType::DynamicBytes) => unimplemented!(),
            Type::C(CompoundType::String) => unimplemented!(),
            Type::NumLiteral(_) => Self {
                indirection: Indirection::RValue,
                ty,
                data: Rc::new(RefCell::new(Data::Literal)),
            },
        };
        data.sanity_check();
        data
    }

    /// The function deals with the situation where
    /// we need to assign a value to a variable.
    ///     i.e., [ty] [storage] var = [self];
    /// is_lvalue is true if self is a lvalue, false otherwise.
    pub fn assign_to(&self, ty: Ty, indirection: Indirection) -> Result<Self, String> {
        self.sanity_check();

        if !Type::implicit_conversion(&self.ty, &ty) {
            return Err(format!(
                "Type mismatch: cannot assign {:?} to {:?}",
                self.ty, ty
            ));
        }

        let rhs = match &*self.ty {
            Type::NumLiteral(num) => Self {
                indirection: Indirection::RValue,
                ty: ty.clone(),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::Num(
                    num.numerator.clone(),
                )))),
            },
            _ => self.clone(),
        };

        rhs.sanity_check();

        if rhs.indirection == Indirection::RValue && indirection == Indirection::Ref {
            return Err("Cannot assign rvalue to ref".to_string());
        }

        if rhs.indirection == Indirection::RValue || indirection == Indirection::Ref {
            Ok(Self {
                indirection,
                ty,
                data: rhs.data.clone(),
            })
        } else {
            Ok(Self {
                indirection,
                ty,
                data: Rc::new(RefCell::new(rhs.data.borrow().deep_copy())),
            })
        }
    }

    pub fn assign_from(&mut self, rhs: &Self) -> Result<(), String> {
        self.sanity_check();
        rhs.sanity_check();

        if !Type::implicit_conversion(&rhs.ty, &self.ty) {
            return Err(format!(
                "Type mismatch: cannot assign {:?} to {:?}",
                rhs.ty, self.ty
            ));
        }

        let rhs = match &*rhs.ty {
            Type::NumLiteral(num) => Self {
                indirection: Indirection::RValue,
                ty: self.ty.clone(),
                data: Rc::new(RefCell::new(Data::Basic(Primitive::Num(
                    num.numerator.clone(),
                )))),
            },
            _ => rhs.clone(),
        };

        rhs.sanity_check();

        if self.indirection == Indirection::RValue {
            return Err("Cannot assign to rvalue".to_string());
        }

        if self.indirection == Indirection::Ref {
            // We need to store the reference in the data cell.
            // This is a TODO.
            unimplemented!()
        } else {
            *self.data.borrow_mut() = rhs.data.borrow().deep_copy();
        }

        Ok(())
    }

    pub fn new_array(ty: Ty, data: Vec<Self>) -> Self {
        let value_ty = match &*ty {
            Type::C(CompoundType::Array { value, .. }) => value.clone(),
            _ => unreachable!(),
        };

        let data = data
            .into_iter()
            .map(|d| {
                d.assign_to(value_ty.clone(), Indirection::LValue)
                    .unwrap()
                    .data
            })
            .collect();

        let data = Self {
            indirection: Indirection::RValue,
            ty,
            data: Rc::new(RefCell::new(Data::Array(data))),
        };

        data.sanity_check();
        data
    }

    pub fn number_literal_to_data(num: String) -> Result<Self, String> {
        let frac = if num.starts_with("0x") {
            // case 1: num starts with "0x", it is a hex number
            let num = num.trim_start_matches("0x").replace("_", "");
            Fraction::new_from_hex_num_literal(num)?
        } else {
            // case 2: num is a decimal number
            Fraction::new_from_number_literal(num)?
        };

        let data = Self {
            indirection: Indirection::RValue,
            ty: Rc::new(Type::NumLiteral(frac)),
            data: Rc::new(RefCell::new(Data::Literal)),
        };

        data.sanity_check();
        Ok(data)
    }

    pub fn extract_number_literal(&self) -> Option<Fraction> {
        match &*self.ty {
            Type::NumLiteral(num) => Some(num.clone()),
            _ => None,
        }
    }

    /// builtin members:
    ///    - array.length
    ///
    ///    - struct.fields
    pub fn extract_member(&self, name: String) -> Result<Self, String> {
        match &*self.ty {
            Type::C(CompoundType::Array { .. }) => {
                if let Data::Array(arr) = &*self.data.borrow() {
                    if name == "length" {
                        return Ok(Self {
                            indirection: Indirection::RValue,
                            ty: Rc::new(Type::B(BasicType::Uint(256))),
                            data: Rc::new(RefCell::new(Data::Basic(Primitive::from_integer(
                                arr.len() as u32,
                            )))),
                        });
                    }
                    Err(format!("Array does not have member {}", name))
                } else {
                    unreachable!()
                }
            }
            Type::C(CompoundType::Struct { fields, .. }) => {
                if let Data::Struct(map) = &*self.data.borrow() {
                    for (field_name, ty) in fields.iter() {
                        if field_name == &name {
                            return Ok(DataRef {
                                indirection: Indirection::LValue,
                                ty: ty.clone(),
                                data: map.get(field_name).unwrap().clone(),
                            });
                        }
                    }
                    Err(format!("Struct does not have member {}", name))
                } else {
                    unreachable!()
                }
            }
            _ => Err(format!("Type {:?} does not have member {}", self.ty, name)),
        }
    }

    /// builtin functions:
    ///     - array.push( [data] )
    ///     - array.pop()
    pub fn call_method(&self, name: String, args: Vec<Self>) -> Result<Option<Self>, String> {
        match &*self.ty {
            Type::C(CompoundType::Array { value, .. }) => {
                if name == "push" {
                    let arg = if !args.is_empty() {
                        DataRef::default(value.clone(), Indirection::RValue)
                    } else if args.len() == 1 {
                        args[0].assign_to(value.clone(), Indirection::RValue)?
                    } else {
                        return Err("push() takes exactly 1 argument".to_string());
                    };

                    if let Data::Array(arr) = &mut *self.data.borrow_mut() {
                        arr.push(arg.data);
                    } else {
                        unreachable!()
                    }

                    Ok(None)
                } else if name == "pop" {
                    if !args.is_empty() {
                        return Err("pop() takes no argument".to_string());
                    }

                    if let Data::Array(arr) = &mut *self.data.borrow_mut() {
                        arr.pop().ok_or("pop() on empty array".to_string())?;
                        Ok(None)
                    } else {
                        unreachable!()
                    }
                } else {
                    Err(format!("Array does not have method {}", name))
                }
            }
            _ => Err(format!("Type {:?} does not have method {}", self.ty, name)),
        }
    }

    pub fn subscript(&self, idx: Self) -> Result<Self, String> {
        match &*self.ty {
            Type::C(CompoundType::Array { value, .. }) => {
                let idx =
                    idx.assign_to(Rc::new(Type::B(BasicType::Uint(256))), Indirection::RValue)?;
                let idx = match &*idx.data.borrow() {
                    Data::Basic(Primitive::Num(num)) => num.clone(),
                    _ => unreachable!(),
                };

                if idx.bits() > 32 {
                    return Err("Index out of bound".to_string());
                }

                let idx = idx.iter_u32_digits().next().unwrap_or(0);

                if let Data::Array(arr) = &*self.data.borrow() {
                    if idx >= arr.len() as u32 {
                        return Err("Index out of bound".to_string());
                    }

                    Ok(DataRef {
                        indirection: Indirection::LValue,
                        ty: value.clone(),
                        data: arr[idx as usize].clone(),
                    })
                } else {
                    unreachable!()
                }
            }
            Type::C(CompoundType::Mapping { key, value }) => {
                let idx = idx.assign_to(key.clone(), Indirection::RValue)?;
                let idx = match &*idx.data.borrow() {
                    Data::Basic(idx) => idx.clone(),
                    _ => unreachable!(),
                };

                if let Data::Mapping(map) = &mut *self.data.borrow_mut() {
                    if !map.contains_key(&idx) {
                        map.insert(
                            idx.clone(),
                            DataRef::default(value.clone(), Indirection::RValue).data,
                        );
                    }

                    Ok(DataRef {
                        indirection: Indirection::LValue,
                        ty: value.clone(),
                        data: map.get(&idx).unwrap().clone(),
                    })
                } else {
                    unreachable!()
                }
            }
            _ => Err("Subscript operator is not supported".to_string()),
        }
    }

    pub fn deduct_common_types(data: &[Self]) -> Result<Ty, String> {
        let tys = data.iter().map(|d| d.ty.clone()).collect::<Vec<_>>();
        Type::deduct_common_types(tys)
    }

    // TODO: can create a macro for vanity_check
    fn sanity_check(&self) {
        if let (Indirection::Ref, Type::B(_)) = (self.indirection, &*self.ty) {
            panic!("Ref cannot be a basic type");
        }
        self.ty.sanity_check();
        self.data.borrow().sanity_check(self.ty.clone());
    }

    pub fn op(&self, op: Operator, rhs: &Self) -> Result<Self, String> {
        match (&*self.ty, &*rhs.ty) {
            (Type::NumLiteral(num1), Type::NumLiteral(num2)) => {
                let result = match op {
                    Operator::Add => num1 + num2,
                    Operator::Sub => num1 - num2,
                    Operator::Mul => num1 * num2,
                    Operator::Div => num1 / num2,
                    _ => unimplemented!(),
                };

                Ok(Self {
                    indirection: Indirection::RValue,
                    ty: Rc::new(Type::NumLiteral(result)),
                    data: Rc::new(RefCell::new(Data::Literal)),
                })
            }

            (
                Type::B(BasicType::Uint(_)) | Type::B(BasicType::Int(_)) | Type::NumLiteral(_),
                Type::B(BasicType::Uint(_)) | Type::B(BasicType::Int(_)) | Type::NumLiteral(_),
            ) => {
                let ty = Type::deduct_common_types(vec![self.ty.clone(), rhs.ty.clone()])?;
                let lhs = self.assign_to(ty.clone(), Indirection::RValue)?;
                let rhs = rhs.assign_to(ty.clone(), Indirection::RValue)?;

                let lhs = match &*lhs.data.borrow() {
                    Data::Basic(Primitive::Num(b)) => b.clone(),
                    _ => unreachable!(),
                };

                let rhs = match &*rhs.data.borrow() {
                    Data::Basic(Primitive::Num(b)) => b.clone(),
                    _ => unreachable!(),
                };

                let res = match op {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    Operator::Div => lhs / rhs,
                    _ => unimplemented!(),
                };

                let primitive = Primitive::Num(res);
                if match &*ty {
                    Type::B(b) => primitive.overflow(b),
                    _ => unreachable!(),
                } {
                    Err("overflow".to_string())
                } else {
                    Ok(Self {
                        indirection: Indirection::RValue,
                        ty,
                        data: Rc::new(RefCell::new(Data::Basic(primitive))),
                    })
                }
            }

            _ => unimplemented!(),
        }
    }
}

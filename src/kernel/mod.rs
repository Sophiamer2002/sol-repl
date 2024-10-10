use std::{collections::{HashMap, HashSet}, rc::Rc};

use prelude::*;

use solang_parser::pt::{self, Expression, Identifier, StructDefinition, VariableDeclaration};

#[derive(Clone, Debug)]
pub enum ParseUnit {
    Nothing(),
    VariableDefinition(VariableDeclaration, Option<Expression>),
    Expression(Expression),
    StructDefinition(StructDefinition),
}

#[derive(Clone, Debug)]
pub enum Entry {
    Type(Rc<Type>),
    Data(DataRef)
}

#[derive(Clone, Debug)]
pub struct Kernel {
    symbol_table: HashMap<String, Entry>,

    type_table: HashSet<Rc<Type>>,
}

pub enum EvalResult {
    Void(),
    Data(DataRef),
}

impl Kernel {
    pub fn new() -> Self {
        let mut type_table = HashSet::new();
        type_table.insert(Rc::new(Type::B(BasicType::Bool())));
        
        for i in (8..=256).step_by(8) {
            type_table.insert(Rc::new(Type::B(BasicType::Int(i))));
            type_table.insert(Rc::new(Type::B(BasicType::Uint(i))));
        }

        for i in 1..=32 {
            type_table.insert(Rc::new(Type::B(BasicType::Bytes(i))));
        }

        type_table.insert(Rc::new(Type::B(BasicType::Address(true))));
        type_table.insert(Rc::new(Type::B(BasicType::Address(false))));
        type_table.insert(Rc::new(Type::C(CompoundType::String)));
        type_table.insert(Rc::new(Type::C(CompoundType::DynamicBytes)));
        Kernel {
            symbol_table: HashMap::new(),
            type_table,
        }
    }

    pub fn run(&mut self, input: ParseUnit) -> Result<Option<String>, String> {
        match input {
            ParseUnit::Nothing() => Ok(None),
            ParseUnit::VariableDefinition(decl, init) => {
                let VariableDeclaration { name, ty, storage, .. } = decl;
                let name = name.unwrap().name;
                if self.lookup(&name).is_some() {
                    return Err(format!("Identifier {} already defined", name));
                }
                let ty = self.get_type(ty)?;
                match (storage, &*ty) {
                    (Some(_), Type::C(_)) => {
                        if let Some(init) = init {
                            match self.eval(init)? {
                                EvalResult::Data(data_ref) => {
                                    let data_ref = data_ref.assign_to(ty, Indirection::Ref)?;
                                    self.symbol_table.insert(name, Entry::Data(data_ref));
                                    Ok(None)
                                },
                                _ => {
                                    Err("Invalid initializer".to_string())
                                }
                            }
                        } else {
                            Err("Expected initializer for reference type".to_string())
                        }
                    },
                    (Some(_), Type::B(_)) => {
                        Err("Data location can be only specified for array, struct or mapping types".to_string())
                    },
                    (None, _) => {
                        let data = if let Some(init) = init {
                            match self.eval(init)? {
                                EvalResult::Data(data) => {
                                    data.assign_to(ty, Indirection::LValue)?
                                },
                                _ => return Err("Invalid initializer".to_string()),
                            }
                        } else {
                            DataRef::default(ty.clone(), Indirection::LValue)
                        };

                        self.symbol_table.insert(name, Entry::Data(data));
                        Ok(None)
                    },
                    (_, Type::NumLiteral(_)) => panic!("Literal type cannot be in the type table"),
                }
            },
            ParseUnit::Expression(expr) => {
                match self.eval(expr)? {
                    EvalResult::Void() => Ok(None),
                    EvalResult::Data(data) => {
                        Ok(Some(format!("{:?}", data)))
                    },
                }
            },
            ParseUnit::StructDefinition(def) => {
                let StructDefinition { name, fields, .. } = def;
                let ty = Type::C(CompoundType::Struct {
                    identifier: name.unwrap().name.clone(),
                    fields: fields.into_iter().map(|f| {
                        let VariableDeclaration { name, ty, storage, .. } = f;
                        if let Some(_) = storage {
                            return Err("Expected identifier".to_string());
                        }
                        self.get_type(ty).map(|ty| (name.unwrap().name, ty))
                    }).collect::<Result<Vec<_>, _>>()?,
                });
                self.get_or_insert_type(ty);
                Ok(None)
            }
        }
    }

    fn eval(&mut self, expr: Expression) -> Result<EvalResult, String> {
        match expr {
            Expression::Type(_, ty) => {
                unimplemented!()
            },
            Expression::Variable(Identifier { name, .. }) => {
                if let Some(Entry::Data(data_ref)) = self.lookup(&name) {
                    // TODO: data may also be a reference to type
                    //       now we directly parse a type variable
                    //       in a function call
                    Ok(EvalResult::Data(data_ref.clone()))
                } else {
                    Err(format!("Identifier {} not found", name))
                }
            },
            Expression::Parenthesis(_, expr) => {
                self.eval(*expr)
            },
            
            // Literals

            Expression::NumberLiteral(_, num, _what_is_this, None) => {
                // TODO: not knowing how the identifier is used
                if !_what_is_this.is_empty() {
                    unimplemented!()
                }
                let data = DataRef::number_literal_to_data(num)?;
                Ok(EvalResult::Data(data))
            },

            Expression::HexNumberLiteral(_, num, None) => {
                let data = DataRef::number_literal_to_data(num)?;
                Ok(EvalResult::Data(data))
            },

            Expression::ArrayLiteral(_, exprs) => {
                if exprs.is_empty() {
                    return Err("Unable to deduce common type for array elements".to_string());
                }
                let results = exprs.into_iter().map(|expr| self.eval(expr)).collect::<Result<Vec<_>, _>>()?;
                if results.iter().any(|r| !matches!(r, EvalResult::Data(_))) {
                    return Err("Invalid array element".to_string());
                }

                let data: Vec<_> = results.into_iter().map(|r| {
                    match r {
                        EvalResult::Data(data_ref) => data_ref,
                        _ => unreachable!(),
                    }
                }).collect();

                let ty = DataRef::deduct_common_types(&data)?;

                let ty = self.get_or_insert_type(Type::C(
                    CompoundType::Array { len: Some(data.len() as u32), value: ty }));
                
                Ok(EvalResult::Data(DataRef::new_array(ty, data)))
            },

            // Operators
            Expression::FunctionCall(_, func, args) => {
                match *func {
                    Expression::Variable(Identifier { name, .. }) => {
                        if let Some(Entry::Type(ty)) = self.lookup(&name) {
                            // explicit type conversion
                            unimplemented!()
                        }

                        Err("Function call not supported".to_string())
                    },
                    // TODO: type conversion
                    // TODO: abi encoding
                    Expression::MemberAccess(_, expr, Identifier { name, .. }) => {
                        // accessing a method
                        let data = self.eval(*expr)?;
                        let args = args.into_iter().map(|arg| {
                            match self.eval(arg) {
                                Ok(EvalResult::Data(data_ref)) => Ok(data_ref),
                                _ => Err("Invalid argument".to_string()),
                            }
                        }).collect::<Result<Vec<_>, _>>()?;
                        match data {
                            EvalResult::Data(data_ref) => {
                                data_ref.call_method(name, args).map(|d| {
                                    d.map(|d| EvalResult::Data(d)).unwrap_or(EvalResult::Void())
                                })
                            },
                            _ => Err("Invalid member access".to_string()),
                        }
                    },
                    _ => Err("Invalid function call or unimplemented".to_string()),
                }
            },

            Expression::MemberAccess(_, expr, Identifier { name , ..}) => {
                // currently, we donnot support accessing method
                // if we are accessing a method, it is directly 
                // evaluated in Expression::FunctionCall

                let result = self.eval(*expr)?;
                match result {
                    EvalResult::Data(data_ref) => {
                        data_ref.extract_member(name).map(|data_ref| {
                            EvalResult::Data(data_ref)
                        })
                    },
                    _ => Err("Invalid member access".to_string()),
                }
            },

            Expression::ArraySubscript(_, expr, Some(idx)) => {
                let idx = match self.eval(*idx)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid array subscript".to_string()),
                };
                let data = match self.eval(*expr)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid array subscript".to_string()),
                };
                data.subscript(idx).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            Expression::ArraySubscript(_, _, None) => {
                Err("Invalid array subscript".to_string())
            },

            Expression::Assign(_, lhs, rhs) => {
                let mut lhs = match self.eval(*lhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid assignment".to_string()),
                };

                let rhs = match self.eval(*rhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid assignment".to_string()),
                };

                lhs.assign_from(&rhs)?;
                Ok(EvalResult::Void())
            },

            Expression::Add(_, lhs, rhs) => {
                let lhs = match self.eval(*lhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid addition".to_string()),
                };

                let rhs = match self.eval(*rhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid addition".to_string()),
                };

                lhs.op(Operator::Add, &rhs).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            Expression::Subtract(_, lhs, rhs) => {
                let lhs = match self.eval(*lhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid subtraction".to_string()),
                };

                let rhs = match self.eval(*rhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid subtraction".to_string()),
                };

                lhs.op(Operator::Sub, &rhs).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            Expression::Multiply(_, lhs, rhs) => {
                let lhs = match self.eval(*lhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid multiplication".to_string()),
                };

                let rhs = match self.eval(*rhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid multiplication".to_string()),
                };

                lhs.op(Operator::Mul, &rhs).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            Expression::Divide(_, lhs, rhs) => {
                let lhs = match self.eval(*lhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid division".to_string()),
                };

                let rhs = match self.eval(*rhs)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid division".to_string()),
                };

                lhs.op(Operator::Div, &rhs).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            Expression::Negate(_, expr) => {
                let zero = DataRef::number_literal_to_data("0".to_string()).unwrap();
                let expr = match self.eval(*expr)? {
                    EvalResult::Data(data_ref) => data_ref,
                    _ => return Err("Invalid negation".to_string()),
                };

                zero.op(Operator::Sub, &expr).map(|data_ref| {
                    EvalResult::Data(data_ref)
                })
            },

            _ => unimplemented!()
        }
    }

    // the result type must be in the type table
    fn get_type(&mut self, ty: Expression) -> Result<Rc<Type>, String> {
        match ty {
            Expression::Type(_, ty) => {
                match ty {
                    pt::Type::Address => Ok(self.get_basic_type(BasicType::Address(false))),
                    pt::Type::AddressPayable => Ok(self.get_basic_type(BasicType::Address(true))),
                    pt::Type::Bool => Ok(self.get_basic_type(BasicType::Bool())),
                    pt::Type::Int(sz) => Ok(self.get_basic_type(BasicType::Int(sz))),
                    pt::Type::Uint(sz) => Ok(self.get_basic_type(BasicType::Uint(sz))),
                    pt::Type::Bytes(sz) => Ok(self.get_basic_type(BasicType::Bytes(sz))),
                    pt::Type::Mapping { key, value, .. } => {
                        let key = self.get_type(*key)?;
                        if matches!(&*key, Type::C(_)) {
                            return Err("Invalid key type".to_string());
                        }
                        let value = self.get_type(*value)?;
                        Ok(self.get_or_insert_type(Type::C(CompoundType::Mapping { key, value })))
                    },
                    _ => Err("Not supported type or not implemented".to_string()),
                }
            },
            Expression::ArraySubscript(_, expr, None) => {
                let ty = self.get_type(*expr)?;
                Ok(self.get_or_insert_type( Type::C(CompoundType::Array { len: None, value: ty }) ))
            },
            Expression::ArraySubscript(_, expr, Some(sz)) => {
                // idx should be a integer literal or constant value
                // now we only support integer literal
                let ty = self.get_type(*expr)?;
                let sz = self.eval(*sz)?;
                let sz = match sz {
                    EvalResult::Data(data_ref) => {
                        data_ref.extract_number_literal().ok_or("Invalid array size".to_string())?
                    },
                    _ => return Err("Invalid array size".to_string()),
                };
                if let Some(len) = sz.to_u32() {
                    // TODO: check if the type is in the type table for system safety
                    // The current requirement of not equal to (!0) is to ensure 
                    // the comparison in data.rs is correct because we use (!0) as
                    // infinity in the array size
                    if len == 0 || len == (!0) {
                        return Err("Invalid array size".to_string());
                    }
                    Ok(self.get_or_insert_type( Type::C(CompoundType::Array { len: Some(len), value: ty }) ))
                } else {
                    Err("Invalid array size".to_string())
                }
            },
            Expression::Variable(identifier) => {
                let entry = self.lookup(&identifier.name);
                if entry.is_none() {
                    return Err(format!("Type {} not found", identifier.name));
                }

                match entry.unwrap() {
                    Entry::Type(ty) => {
                        // TODO: check if the type is in the type table for system safety
                        // if !self.type_table.contains(ty) {
                        //     panic!("Existing types should exist in the type table.")
                        // }
                        Ok(ty.clone())
                    }
                    _ => Err("Invalid type".to_string()),
                }
            },
            _ => Err("Invalid type".to_string()),
        }
    }

    fn lookup(&mut self, name: &str) -> Option<&mut Entry> {
        self.symbol_table.get_mut(name)
    }

    fn get_or_insert_type(&mut self, ty: Type) -> Ty {
        if let Some(t) = self.type_table.get(&ty) {
            t.clone()
        } else {
            let t = Rc::new(ty);
            self.type_table.insert(t.clone());
            t
        }
    }

    fn get_basic_type(&self, ty: BasicType) -> Ty {
        self.type_table.get(&Type::B(ty)).unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type() {
        let mut table = Kernel::new();

        table.type_table.insert(Rc::new(Type::B(BasicType::Int(256))));
        {
            let ty = Rc::new(Type::B(BasicType::Int(256)));
            println!("{}", table.type_table.contains(&ty));
        }

        let ty = Rc::new(Type::B(BasicType::Int(256)));
        let _ = table.type_table.get(&ty).unwrap().clone();
    }

    #[test]
    fn test_symbol_table() {
        let mut table = Kernel::new();
        let data = DataRef::number_literal_to_data(
            "100".to_string(),
        ).unwrap();

        // table.symbol_table.insert("a".to_string(), Entry::Slot(data.clone())).is_none();
        // table.symbol_table.insert("a".to_string(), Entry::Slot(data.clone())).is_some();
        // if let Entry::Data(data) = table.lookup("a").unwrap() {
        //     *data = data2;
        // }
        
        table.symbol_table.insert("a".to_string(), 
            Entry::Data(data.clone())
        );

        dbg!(table.lookup("a"));
    }
}

pub mod types;

pub mod primitive;

pub mod data;

pub mod prelude {
    pub use super::types::*;
    pub use super::primitive::*;
    pub use super::data::*;
}

use std::{collections::{HashMap, HashSet}, rc::Rc};

use prelude::*;

use solang_parser::{parse, pt::{self, Expression, Loc, SourceUnit, SourceUnitPart, Statement, StructDefinition, VariableDeclaration}};

use crate::utils::fraction::Fraction;

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
    Data(Data),
    Compound {
        ty: Rc<Type>,
        loc: Location,
        slot: usize,
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Entry>,
    types: HashSet<Rc<Type>>,

    storage: Storage,
    memory: Memory,
}

pub enum EvalResult {
    Void(),
    NumberLiteral(Fraction),
    DataHandle(Location, usize),
    Identifier(String),
    Data(Data),
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut types = HashSet::new();
        types.insert(Rc::new(Type::B(BasicType::Bool())));
        
        for i in 8..=256 {
            types.insert(Rc::new(Type::B(BasicType::Int(i))));
            types.insert(Rc::new(Type::B(BasicType::Uint(i))));
        }

        for i in 1..=32 {
            types.insert(Rc::new(Type::B(BasicType::Bytes(i))));
        }

        types.insert(Rc::new(Type::B(BasicType::Address(true))));
        types.insert(Rc::new(Type::B(BasicType::Address(false))));
        types.insert(Rc::new(Type::C(CompoundType::String())));
        types.insert(Rc::new(Type::C(CompoundType::DynamicBytes())));
        SymbolTable {
            symbols: HashMap::new(),
            types,
            storage: Storage::new(),
            memory: Memory::new(),
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
                    (Some(pt::StorageLocation::Storage(_)), Type::C(_)) => {
                        if let Some(init) = init {
                            if let EvalResult::DataHandle(Location::Storage, slot) = self.eval(init)? {
                                if self.storage.match_type(slot, &ty) {
                                    self.symbols.insert(name, Entry::Compound { ty, loc: Location::Storage, slot });
                                    return Ok(None)
                                }
                            } 
                        }
                        Err("Invalid initialized data".to_string())
                    },

                    (None, Type::C(CompoundType::Mapping { .. })) => {
                        if init.is_some() {
                            return Err("Invalid initialized data".to_string());
                        }
                        let slot = self.storage.assign_mapping(&ty);
                        self.symbols.insert(name, Entry::Compound { ty, loc: Location::StorageOwn, slot });
                        Ok(None)
                    },

                    (_, Type::C(CompoundType::Mapping { .. })) => {
                        Err("Invalid storage location".to_string())
                    },

                    (Some(pt::StorageLocation::Memory(_)), Type::C(_)) => {
                        Err("Not supported".to_string())
                    },

                    (None, Type::C(CompoundType::Array { .. })) => {
                        let slot;
                        if let Some(expr) = init {
                            unimplemented!()
                        } else {
                            slot = self.storage.assign_array(&ty);
                        }

                        self.symbols.insert(name, Entry::Compound { ty, loc: Location::StorageOwn, slot });
                        Ok(None)
                    },

                    (None, Type::C(CompoundType::Struct { .. })) => {
                        let slot = self.storage.assign_struct(&ty);
                        self.symbols.insert(name, Entry::Compound { ty, loc: Location::StorageOwn, slot });
                        Ok(None)
                    },

                    (Some(_), Type::B(_)) => {
                        Err("Basic type does not support storage location".to_string())
                    },

                    (None, Type::B(basic)) => {
                        let data = if let Some(expr) = init {
                            let data = self.eval_to_data(expr)?;
                            basic.fit(data)?
                        } else {
                            Data::default(basic)
                        };
                        self.symbols.insert(name, Entry::Data(data));
                        Ok(None)
                    },
                    _ => Err("Not implemented".to_string()),
                }
            },
            ParseUnit::Expression(expr) => {
                match self.eval(expr)? {
                    EvalResult::Void() => Ok(None),
                    EvalResult::Data(data) => Ok(Some(data.to_string())),
                    EvalResult::DataHandle(Location::Memory, slot) => {
                        Ok(Some(format!("{:?}", self.memory.get(slot).unwrap())))
                    },
                    EvalResult::DataHandle(Location::Storage, slot) => {
                        Ok(Some(format!("{:?}", self.storage.get(slot).unwrap())))
                    },
                    EvalResult::DataHandle(Location::StorageOwn, slot) => {
                        Ok(Some(format!("{:?}", self.storage.get(slot).unwrap())))
                    },
                    EvalResult::Identifier(name) => {
                        match self.lookup(&name) {
                            Some(Entry::Data(data)) => Ok(Some(data.to_string())),
                            Some(Entry::Compound { ty, .. }) => {
                                Ok(Some(format!("{:?}", ty)))
                            },
                            _ => Err("Not matched".to_string()),
                        }
                    },
                    EvalResult::NumberLiteral(num) => {
                        Ok(Some(num.to_string()))
                    },
                    _ => Err("Not matched".to_string()),
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
        unimplemented!()
    }

    fn eval_to_data(&mut self, expr: Expression) -> Result<Data, String> {
        let result = self.eval(expr)?;
        match result {
            EvalResult::Data(data) => Ok(data),
            EvalResult::DataHandle(Location::Memory, slot) => {
                if let Some(data) = self.memory.get_data(slot) {
                    Ok(data.clone())
                } else {
                    Err("Not matched".to_string())
                }
            },
            EvalResult::Identifier(name) => {
                if let Some(Entry::Data(data)) = self.lookup(&name) {
                    Ok(data.clone())
                } else {
                    Err("Not matched".to_string())
                }
            },
            EvalResult::NumberLiteral(num) => {
                number_literal_to_data(&num)
            },
            _ => Err("Not matched".to_string()),
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
                    EvalResult::NumberLiteral(sz) => sz,
                    _ => return Err("Invalid array size".to_string()),
                };
                if let Some(len) = sz.to_u32() {
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
                    Entry::Type(ty) => Ok(ty.clone()),
                    _ => Err("Invalid type".to_string()),
                }
            },
            _ => Err("Invalid type".to_string()),
        }
    }

    fn lookup(&mut self, name: &str) -> Option<&mut Entry> {
        self.symbols.get_mut(name)
    }

    fn get_or_insert_type(&mut self, ty: Type) -> Rc<Type> {
        if let Some(t) = self.types.get(&ty) {
            t.clone()
        } else {
            let t = Rc::new(ty);
            self.types.insert(t.clone());
            t
        }
    }

    fn get_basic_type(&mut self, ty: BasicType) -> Rc<Type> {
        self.types.get(&Type::B(ty)).unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::*;

    #[test]
    fn test_type() {
        let mut table = SymbolTable::new();

        table.types.insert(Rc::new(Type::B(BasicType::Int(256))));
        {
            let ty = Rc::new(Type::B(BasicType::Int(256)));
            println!("{}", table.types.contains(&ty));
        }

        let ty = Rc::new(Type::B(BasicType::Int(256)));
        let _ = table.types.get(&ty).unwrap().clone();
    }

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();
        let data = Data::Int(Int {
            size: 32,
            value: BigInt::from(100),
        });

        let data2 = Data::Int(Int {
            size: 16,
            value: BigInt::from(200),
        });
        table.symbols.insert("a".to_string(), Entry::Data(data.clone())).is_none();
        table.symbols.insert("a".to_string(), Entry::Data(data.clone())).is_some();
        if let Entry::Data(data) = table.lookup("a").unwrap() {
            *data = data2;
        }

        dbg!(table.lookup("a"));
    }
}

pub mod types;

pub mod data;

pub mod storage;

pub mod prelude {
    pub use super::types::*;
    pub use super::data::*;
    pub use super::storage::*;
}

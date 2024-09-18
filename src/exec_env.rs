use std::rc::Rc;

use solang_parser::{parse, pt::{self, Expression, SourceUnit, SourceUnitPart, Statement, StructDefinition, VariableDeclaration}};

use crate::primitives::prelude::*;

#[derive(Clone, Debug)]
pub struct ExecEnv {
    type_table: TypeTable,
    symbol_table: SymbolTable,
}

#[derive(Clone, Debug)]
pub enum ParseUnit {
    Nothing(),
    VariableDefinition(VariableDeclaration, Option<Expression>),
    Expression(Expression),
    StructDefinition(StructDefinition),
}

#[derive(Clone, Debug, Default)]
pub struct ExecResult {
    pub message: Option<String>,
}

pub struct ExecError {
    pub message: String,
}

impl From<String> for ExecError {
    fn from(message: String) -> Self {
        ExecError { message }
    }
}

impl ExecEnv {
    pub fn new() -> Self {
        ExecEnv {
            type_table: TypeTable::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn execute(&mut self, input: String) -> Result<ExecResult, ExecError> {
        match parse_input(input) {
            Ok(parsed) => {
                self.run(parsed)
            },
            Err(err) => {
                Err(ExecError { message: err })
            }
        }
    }

    fn run(&mut self, input: ParseUnit) -> Result<ExecResult, ExecError> {
        match input {
            ParseUnit::Nothing() => {
                Ok(ExecResult::default())
            },
            ParseUnit::VariableDefinition(decl, expr) => {
                let data = expr.map(|expr| self.eval_expression(expr)).transpose()?;
                let VariableDeclaration {loc: _, ty, storage: _, name} = decl;
                let name = name.unwrap().name;
                let ty = self.get_type(ty)?;
                self.add_variable(name, ty, data)?;
                Ok(ExecResult::default())
            },
            ParseUnit::Expression(expr) => {
                let data = self.eval_expression(expr)?;
                Ok(ExecResult { message: Some(data.to_string())})
            },
            ParseUnit::StructDefinition(def) => {
                let StructDefinition {loc: _, name, fields} = def;
                let name = name.unwrap().name;
                let fields = fields.into_iter().map(|decl| {
                    let VariableDeclaration {loc: _, ty, storage: _, name} = decl;
                    let name = name.unwrap().name;
                    self.get_type(ty).map(|ty| (name, ty))
                }).collect::<Result<Vec<_>, _>>()?;
                let ty = Type::Struct { identifier: name.clone(), fields };
                self.add_struct_type(name, ty)?;
                Ok(ExecResult::default())
            },
        }
    }

    fn eval_expression(&mut self, expr: Expression) -> Result<Data, String> {
        unimplemented!()
    }

    // the return pointer must be in the type table
    fn get_type(&mut self, ty: Expression) -> Result<Rc<Type>, String> {
        match ty {
            Expression::Type(_, ty) => {
                match ty {
                    pt::Type::Address => Ok(self.type_table.get_type(&Type::Basic(BasicType::Address(false))).unwrap()),
                    pt::Type::AddressPayable => Ok(self.type_table.get_type(&Type::Basic(BasicType::Address(true))).unwrap()),
                    pt::Type::Bool => Ok(self.type_table.get_type(&Type::Basic(BasicType::Bool())).unwrap()),
                    pt::Type::Bytes(sz) => Ok(self.type_table.get_type(&Type::Basic(BasicType::Bytes(Some(sz)))).unwrap()),
                    pt::Type::Int(i) => Ok(self.type_table.get_type(&Type::Basic(BasicType::Int(i))).unwrap()),
                    pt::Type::Uint(i) => Ok(self.type_table.get_type(&Type::Basic(BasicType::Uint(i))).unwrap()),
                    pt::Type::String => Ok(self.type_table.get_type(&Type::Basic(BasicType::String())).unwrap()),
                    pt::Type::DynamicBytes => Ok(self.type_table.get_type(&Type::Basic(BasicType::Bytes(None))).unwrap()),
                    pt::Type::Mapping { loc: _, key, key_name: _, value, value_name: _ } => {
                        let key = self.get_basic_type(*key)?;
                        let value = self.get_type(*value)?;
                        Ok(self.type_table.get_or_insert(Type::Mapping { key, value }))
                    },
                    _ => Err("Not implemented".to_string()),
                }
            },
            Expression::ArraySubscript(_, expr, None) => {
                let ty = self.get_type(*expr)?;
                Ok(self.type_table.get_or_insert(Type::Array { len: None, value: ty }))
            },
            Expression::ArraySubscript(_, expr, Some(sz)) => {
                // idx should be a integer literal or constant value
                // now we only support integer literal
                let ty = self.get_type(*expr)?;
                let sz = self.eval_expression(*sz)?;
                let sz = match sz {
                    Data::NumberLiteral(n) => n,
                    _ => return Err("Invalid array size".to_string()),
                };
                if let Some(len) = sz.to_u32() {
                    Ok(self.type_table.get_or_insert(Type::Array { len: Some(len), value: ty }))
                } else {
                    Err("Invalid array size".to_string())
                }
            },
            Expression::Variable(identifier) => {
                let entry = self.symbol_table.lookup(&identifier.name);
                if entry.is_none() {
                    return Err(format!("Variable {} not found", identifier.name));
                }

                match entry.unwrap() {
                    Entry::Type(ty) => Ok(ty.clone()),
                    _ => Err("Invalid type".to_string()),
                }
            },
            _ => Err("Invalid type".to_string()),
        }
    }

    fn get_basic_type(&self, ty: Expression) -> Result<Rc<Type>, String> {
        let ty = match ty {
            Expression::Type(_, ty) => {
                match ty {
                    pt::Type::Address => BasicType::Address(false),
                    pt::Type::AddressPayable => BasicType::Address(true),
                    pt::Type::Bool => BasicType::Bool(),
                    pt::Type::Bytes(sz) => BasicType::Bytes(Some(sz)),
                    pt::Type::Int(i) => BasicType::Int(i),
                    pt::Type::Uint(i) => BasicType::Uint(i),
                    pt::Type::String => BasicType::String(),
                    pt::Type::DynamicBytes => BasicType::Bytes(None),
                    _ => return Err("Not a basic type".to_string()),
                }
            },
            // currently do not support alias for basic types
            _ => return Err("Invalid type".to_string()),
        };

        Ok(self.type_table.get_type(&Type::Basic(ty)).unwrap())
    }

    fn get_type_from_data(&self, data: &Data) -> Rc<Type> {
        match data {
            Data::Basic(basic) => {
                let ty = match basic {
                    BasicData::Bool(_) => BasicType::Bool(),
                    BasicData::Int(Int{ size, value: _}) => BasicType::Int((*size as u16) * 8),
                    BasicData::Uint(Uint{ size, value: _}) => BasicType::Uint((*size as u16) * 8),
                    BasicData::Bytes(Bytes{ size, value: _}) => BasicType::Bytes(size.clone()),
                    BasicData::Address(Address{ payable, value: _}) => BasicType::Address(*payable),
                    BasicData::StringQ(_) => BasicType::String(),
                };
                self.type_table.get_type(&Type::Basic(ty)).unwrap()
            },
            Data::Array(a) => a.ty.clone(),
            Data::Struct(s) => s.ty.clone(),
            Data::Mapping(m) => m.ty.clone(),
            Data::NumberLiteral(_) => self.type_table.get_type(&Type::Number()).unwrap(),
            Data::Void() => unimplemented!()
        }
    }

    fn add_variable(&mut self, identifier: String, ty: Rc<Type>, data: Option<Data>) -> Result<(), String> {
        let entry = self.symbol_table.lookup(&identifier);
        if entry.is_some() {
            return Err(format!("Variable {} already exists", identifier));
        }

        let data = if let Some(data) = data {
            if self.get_type_from_data(&data) == ty {
                data
            } else {
                implicit_conversion(&ty, &data)?
            }
        } else {
            get_default_data(&ty)
        };
        
        self.symbol_table.insert_variable(identifier, data)
    }

    fn add_struct_type(&mut self, identifier: String, ty: Type) -> Result<(), String> {
        assert!(matches!(ty, Type::Struct { .. }));
        let ty = self.type_table.get_or_insert(ty);
        self.symbol_table.insert_type(identifier, ty)
    }
}

fn parse_input(input: String) -> Result<ParseUnit, String> {
    match parse(&input, 0) {
        Ok((SourceUnit(parts), _comments)) => {
            let part = parts.into_iter().next();
            if part.is_none() {
                return Ok(ParseUnit::Nothing());
            }
            
            let part = part.unwrap();
            match part {
                SourceUnitPart::StructDefinition(def) => {
                    return Ok(ParseUnit::StructDefinition(*def));
                },
                _ => {}
            }
        },
        Err(_) => {},
    }

    let wrapped_input = format!(
        "function __sol_repl_94023() public {{ {} }}",
        input
    );

    match parse(&wrapped_input, 0) {
        Ok((SourceUnit(parts), _comments)) => {
            if parts.len() == 1 {
                let part = parts.into_iter().next().unwrap();
                let statements = match part {
                    SourceUnitPart::FunctionDefinition(def) => {
                        if let Some(Statement::Block{
                            loc:_, unchecked: b, statements: c
                        }) = def.body {
                            assert!(!b);
                            Some(c)
                        } else { None }
                    },
                    _ => None
                };
                if let Some(statements) = statements {
                    if statements.len() == 1 {
                        let statement: Statement  = statements.into_iter().next().unwrap();
                        if let Statement::Expression(_, expr) = statement {
                            return Ok(ParseUnit::Expression(expr));
                        } else if let Statement::VariableDefinition(_, decl, expr) = statement {
                            return Ok(ParseUnit::VariableDefinition(decl, expr));
                        }
                    }
                }
            }
        },
        Err(_) => {},
    }

    return Err("Cannot parse input".to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "struct Funder { address addr; uint amount; } // abcde";
    const INPUT2: &str = "uint[a][] inspector = bytes(abi.encode(a+b));";

    #[test]
    fn test_parse_input() {
        let input = INPUT1;
        let parsed = parse_input(input.to_string()).unwrap();
        dbg!(parsed);

        let input = INPUT2;
        let parsed = parse_input(input.to_string()).unwrap();
        assert!(matches!(parsed, ParseUnit::VariableDefinition(_, _)));
        dbg!(parsed);
    }
}

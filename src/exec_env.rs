use std::rc::Rc;

use solang_parser::{parse, pt::{Expression, SourceUnit, SourceUnitPart, Statement, StructDefinition, VariableDeclaration}};

use crate::primitives::prelude::*;

#[derive(Clone, Debug)]
pub struct ExecEnv {
    type_table: TypeTable,
    symbol_table: SymbolTable,
}

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
                let ty = Rc::new(Type::Struct { identifier: name.clone(), fields: fields });
                self.add_struct_type(name, ty)?;
                Ok(ExecResult::default())
            },
        }
    }

    fn eval_expression(&mut self, expr: Expression) -> Result<Data, String> {
        unimplemented!()
    }

    fn get_type(&self, ty: Expression) -> Result<Rc<Type>, String> {
        match ty {
            _ => unimplemented!()
        }
    }

    fn add_variable(&mut self, identifier: String, ty: Rc<Type>, data: Option<Data>) -> Result<(), String> {
        unimplemented!()
    }

    fn add_struct_type(&mut self, identifier: String, ty: Rc<Type>) -> Result<(), String> {
        unimplemented!()
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
                _ => return Err("Not supported".to_string()),
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
                        let statement  = statements.into_iter().next().unwrap();
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

    #[test]
    fn test_parse_input() {
        let x: Option<String> = Default::default();
        dbg!(x);
    }
}

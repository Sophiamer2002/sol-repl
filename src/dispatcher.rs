use solang_parser::{parse, pt::{SourceUnit, SourceUnitPart, Statement}};

use crate::kernel::{Kernel, ParseUnit};

#[derive(Debug)]
pub struct Dispatcher {
    pub kernel: Kernel
}

impl Dispatcher {
    pub fn new() -> Self {
        Dispatcher {
            kernel: Kernel::new()
        }
    }

    pub fn dispatch(&mut self, input: &str) -> Result<String, String> {
        dbg!(input);
        let parsed = parse_input(input.to_string())?;
        self.kernel.run(parsed).map(|m| m.unwrap_or("".to_string()))
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
    
    const _TRICKY_INPUT: [&str; 3] = [
r#"
bytes9 b1;
bytes10[] arr = [b1, hex"001212", hex"000011000000000000"];
// deduce type of arr for hex literal
"#,

r#"
uint x = 255 + [1,2,3][1];
// should overflow because [1,2,3][1] has type uint8
"#,

r#"
uint[] arr = [1, 100, uint8(10), 9091209];
uint[] arr2 = [1,2,3];
uint[][] array = [arr, arr2];
uint[][] array2 = [arr, [1,2,3]]; // error: cannot deduct common types
uint[][] array3 = [[300, 301, 302], [1,2,3]]; // error: cannot deduct common types
uint[][] array4 = [[30, 31, 32, 33], [1,2,3]]; // error: cannot deduct common types
"#,
    ];

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
pub mod dispatcher;

pub mod exec_env;

pub mod primitives;

pub mod utils;

pub mod prelude {
    pub use crate::dispatcher::Dispatcher;
    pub use crate::exec_env::ExecEnv;
}

#[cfg(test)]
mod tests {
    use solang_parser::{lexer::Lexer, parse, pt::{ContractPart, SourceUnitPart}};

    const INPUT: &str = r#"
contract flipper {
    bool private value;

    /// Constructor that initializes the `bool` value to the given `init_value`.
    constructor(bool initvalue) {
        value = initvalue;
    }

    /// A message that can be called on instantiated contracts.
    /// This one flips the value of the stored `bool` from `true`
    /// to `false` and vice versa.
    function flip() public {
        value = !value;
    }

    /// Simply returns the current value of our `bool`.
    function get() public view returns (bool) {
        return value;
    }
}
    "#;


    const INPUT2: &str = "contract test{ uint private a; uint private b; function t() public { a + b; } }";
    const INPUT3: &str = "bytes248 inspector = abi.encode(a+b);";
    const INPUT4: &str = "c.funders[c.numFunders++] = Funder({addr: msg.sender, amount: msg.value});";
    const INPUT5: &str = "struct Funder { address addr; uint amount; } // abcde";
    const INPUT6: &str = "uint[a][] inspector = bytes(abi.encode(a+b));";
    const INPUT7: &str = "bytes test = 123;";
    const INPUT8: &str = "uint256 x = type(uint).max;";

    #[test]
    fn parser_test_1() {
        let (tree, comments) = parse(INPUT, 0).unwrap();

        for part in &tree.0 {
            match part {
                SourceUnitPart::ContractDefinition(def) => {
                    println!("found contract {:?}", def.name);
                    for part in &def.parts {
                        match part {
                            ContractPart::VariableDefinition(def) => {
                                println!("variable {:?}: type {:?}", def.name, def.ty);
                                println!("    all {:?}", def);
                            }
                            ContractPart::FunctionDefinition(def) => {
                                println!("function {:?}", def.name);
                                println!("    all {:?}", def);
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
    }

    #[test]
    fn parser_test_2() {
        let input = format!("function __test() public {{ {} }}", INPUT8);
        println!("input: {}", input);
        let (tree, comments) = parse(&input, 0).unwrap();
        dbg!(tree);
        dbg!(comments);
    }

    #[test]
    fn parser_test_3() {
        let input = INPUT5;
        println!("input: {}", input);
        let (tree, comments) = parse(input, 0).unwrap();
        dbg!(tree);
        dbg!(comments);
    }

    #[test]
    fn lexer_usage_1() {
        let mut comments = Vec::new();
        let mut errors = Vec::new();

        let result = Lexer::new(INPUT, 0, &mut comments, &mut errors);
        for t in result {
            dbg!(t);
        }
    }
}
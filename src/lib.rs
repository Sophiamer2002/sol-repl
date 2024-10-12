pub mod dispatcher;

pub mod kernel;

pub mod utils;

pub mod prelude {
    pub use crate::dispatcher::Dispatcher;
}

#[cfg(test)]
mod tests {
    use solang_parser::{
        lexer::Lexer,
        parse,
        pt::{ContractPart, SourceUnitPart},
    };

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

    const _INPUT2: &str =
        "c_ontract test{ uint private a; uint private b; function t() public { a + b; } }";
    const _INPUT3: &str = "bytes248 inspector = abi.encode(a+b);";
    const _INPUT4: &str =
        "c.funders[c.numFunders++] = Funder({addr: msg.sender, amount: msg.value});";
    const INPUT5: &str = "struct Funder { address addr; uint amount; } // abcde";
    const _INPUT6: &str = "uint[a][] inspector = bytes(abi.encode.c(a+b));";
    const _INPUT7: &str = "uint test = 123_456_789;";
    const INPUT8: &str = "uint test = 0x123_392_abc;";
    const _INPUT9: &str = "uint256 x = type(uint).max;";
    const _INPUT10: &str = "bytes test = hex\"1234123\";";
    const _INPUT11: &str = "bytes test = new int[];";

    #[test]
    fn parser_test_1() {
        let (tree, _comments) = parse(INPUT, 0).unwrap();

        for part in &tree.0 {
            if let SourceUnitPart::ContractDefinition(def) = part {
                println!("found contract {:?}", def.name);
                for part in &def.parts {
                    match part {
                        ContractPart::VariableDefinition(def) => {
                            println!("variable {:?}: type {:?}", def.name, def.ty);
                            println!("    all {def:?}");
                        }
                        ContractPart::FunctionDefinition(def) => {
                            println!("function {:?}", def.name);
                            println!("    all {def:?}");
                        }
                        _ => (),
                    }
                }
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

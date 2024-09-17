use std::{collections::HashSet, rc::Rc};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Bool(),
    Int(u16),
    Uint(u16),
    Bytes(Option<u8>), // None means dynamic
    Address(bool), // payable or not
    String(),
    Struct {
        identifier: String,
        fields: Vec<(String, Rc<Type>)>
    },
    Mapping {
        key: Rc<Type>, 
        value: Rc<Type>
    },
    Array {
        len: Option<usize>,
        value: Rc<Type>
    },
}

#[derive(Clone, Debug)]
pub struct TypeTable {
    pub table: HashSet<Rc<Type>>,
}

impl TypeTable {
    pub fn new() -> Self {
        let mut table = HashSet::new();
        table.insert(Rc::new(Type::Bool()));

        for i in 8..=256 {
            table.insert(Rc::new(Type::Int(i)));
            table.insert(Rc::new(Type::Uint(i)));
        }

        table.insert(Rc::new(Type::Bytes(None)));
        for i in 1..=32 {
            table.insert(Rc::new(Type::Bytes(Some(i))));
        }

        table.insert(Rc::new(Type::Address(false)));
        table.insert(Rc::new(Type::Address(true)));

        table.insert(Rc::new(Type::String()));
        Self {
            table
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type() {
        let mut table = TypeTable {
            table: HashSet::new(),
        };

        table.table.insert(Rc::new(Type::Int(256)));
        {
            let ty = Rc::new(Type::Int(256));
            println!("{}", table.table.contains(&ty));
        }

        let ty = Rc::new(Type::Int(256));
        let _ = table.table.get(&ty).unwrap().clone();
    }
}
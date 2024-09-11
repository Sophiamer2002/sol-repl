use crate::prelude::*;
use solang_parser::{diagnostics::Diagnostic, parse};

#[derive(Debug)]
pub struct Dispatcher {
    pub env: ExecEnv,
}

#[derive(Debug)]
pub enum DispatchResult {
    Success(Option<String>),
    ParseError(Vec<Diagnostic>),
    ExecuteError(String),
}

impl Dispatcher {
    pub fn new() -> Self {
        Dispatcher {
            env: ExecEnv::new(),
        }
    }

    pub fn dispatch(&mut self, input: &str) -> DispatchResult {
        dbg!(input);
        match parse(input, 0) {
            Ok((tree, _)) => {
                self.env.execute(tree);
                // TODO
                DispatchResult::Success(None)
            },
            Err(e) => {
                DispatchResult::ParseError(e)
            },
        }
    }
}
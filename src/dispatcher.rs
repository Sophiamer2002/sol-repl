use crate::{exec_env::{ExecError, ExecResult}, prelude::*};

#[derive(Debug)]
pub struct Dispatcher {
    pub env: ExecEnv,
}

#[derive(Debug)]
pub enum DispatchResult {
    Success(Option<String>),
    Failure(String),
}

impl Dispatcher {
    pub fn new() -> Self {
        Dispatcher {
            env: ExecEnv::new(),
        }
    }

    pub fn dispatch(&mut self, input: &str) -> DispatchResult {
        dbg!(input);
        match self.env.execute(input.to_string()) {
            Ok(ExecResult { message }) => DispatchResult::Success(message),
            Err(ExecError { message }) => DispatchResult::Failure(message),
        }
    }
}
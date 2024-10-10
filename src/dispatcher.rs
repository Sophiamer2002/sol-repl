use crate::{exec_env::{ExecError, ExecResult}, prelude::*};

#[derive(Debug)]
pub struct Dispatcher {
    // pub env: ExecEnv,
}

#[derive(Debug)]
pub enum DispatchResult {
    Success(Option<String>),
    Failure(String),
}

impl Dispatcher {
    pub fn new() -> Self {
        Dispatcher {
            // env: ExecEnv::new(),
        }
    }

    pub fn dispatch(&mut self, input: &str) -> DispatchResult {
        dbg!(input);
        // match self.env.execute(input.to_string()) {
            // Ok(ExecResult { message }) => DispatchResult::Success(message),
            // Err(ExecError { message }) => DispatchResult::Failure(message),
        // }
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    
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
}
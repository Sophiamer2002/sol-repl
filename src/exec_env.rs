use solang_parser::pt::SourceUnit;

#[derive(Clone, Debug)]
pub struct ExecEnv {

}

pub enum ExecResult {
    Success(Option<String>),
    Error(String),
}

impl ExecEnv {
    pub fn new() -> Self {
        ExecEnv {}
    }

    pub fn execute(&mut self, tree: SourceUnit) ->  ExecResult {
        match tree.0 {
            _ => {}
        }
        ExecResult::Success(None)
    }
}
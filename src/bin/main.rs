use rustyline::DefaultEditor;
use sol_repl::prelude::*;

fn main() {
    let mut dispatcher = Dispatcher::new();

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let command = rl.readline(">> ");

        match command {
            Ok(line) => dispatcher.dispatch(&line),
            Err(_) => break,
        };
    }
}
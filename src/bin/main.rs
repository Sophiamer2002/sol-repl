use rustyline::DefaultEditor;
use sol_repl::prelude::*;

fn main() {
    let mut dispatcher = Dispatcher::new();

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let command = rl.readline(">> ");

        match command {
            Ok(line) => {
                let result = dispatcher.dispatch(&line);
                match result {
                    Ok(message) => {
                        println!("{}", message);
                    }
                    Err(message) => {
                        println!("Failure: {}", message);
                    }
                }
            }
            Err(_) => break,
        };
    }
}
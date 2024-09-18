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
                    DispatchResult::Success(message) => {
                        if let Some(message) = message {
                            println!("{}", message);
                        }
                    }
                    DispatchResult::Failure(message) => {
                        println!("Failure: {}", message);
                    }
                }
            }
            Err(_) => break,
        };
    }
}
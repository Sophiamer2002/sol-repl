use std::{
    fs::{File, OpenOptions},
    io::Write,
    path::Path,
};

use chrono::Local;

use super::kernel::{parse_input, Kernel};

#[derive(Debug)]
pub struct Dispatcher {
    pub kernel: Kernel,
    pub file: File,
}

impl Default for Dispatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl Dispatcher {
    pub fn new() -> Self {
        let path = Path::new("target/history.txt");
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)
            .expect("Unable to open history file");

        let now = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
        writeln!(file, "Execution start: {}", now).unwrap();

        Dispatcher {
            kernel: Kernel::new(),
            file,
        }
    }

    pub fn dispatch(&mut self, input: &str) -> Result<String, String> {
        dbg!(input);
        let parsed = parse_input(input.to_string())?;

        // write input to history file
        writeln!(self.file, "{}", input).unwrap();

        self.kernel.run(parsed).map(|m| m.unwrap_or("".to_string()))
    }
}

#[cfg(test)]
mod tests {

    const _TRICKY_INPUT: [&str; 4] = [
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
        r#"
uint x = 1 / 2 + 1 / 3 + 1 / 6;
uint xx = true ? 1: (255 + [[1,2,3]][0][0]);
"#,
    ];
}

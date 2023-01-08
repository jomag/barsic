use std::io::Write;

use crate::parser;

const PROMPT: &str = "] ";

pub struct Terminal {}

impl Terminal {
    fn print(&self, text: &str) {
        print!("{}", text);
        std::io::stdout().flush().unwrap();
    }

    fn readline(&self) -> String {
        let line = std::io::stdin().lines().next().unwrap().unwrap();
        return line;
    }
}

pub fn shell(term: &Terminal) {
    loop {
        term.print(PROMPT);
        let res = term.readline();
        println!("You write: {}", res);
        let res = parser::parse_line(&res);
        match res {
            Ok(line) => match line.num {
                Some(n) => println!("Parsed line number {}", n),
                None => println!("Parsed line"),
            },
            Err(e) => {
                println!("Invalid input: {:?}", e);
            }
        }
    }
}

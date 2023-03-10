use std::fs::File;
use std::io::{prelude::*, BufReader};

use barsic::lex::LexError;
use barsic::parser::{parse_line, ParserError, RuntimeError, Statement, SyntaxError};
use barsic::statements::run_statement::{Context, Program, ProgramError, RunStatement};
use barsic::support::Support;

// use barsic::shell::Terminal;

#[derive(Debug)]
enum TestError {
    Lex(LexError),
    Syntax(SyntaxError),
    Program(ProgramError),
    Runtime(RuntimeError),
}

#[derive(Debug)]
struct TestCase {
    name: String,
    source: Vec<String>,
    expected_output: Vec<String>,
}

struct IntegrationSupport {
    output: Vec<String>,
    error_output: Vec<String>,
}

impl IntegrationSupport {
    fn new() -> Self {
        Self {
            output: vec![],
            error_output: vec![],
        }
    }
}

impl Support for IntegrationSupport {
    fn print(&mut self, channel: usize, message: &str) {
        assert!(channel == 123);
        self.output.push(message.into());
    }

    fn print_error(&mut self, message: &str) {
        self.error_output.push(message.into());
    }
}

impl TestCase {
    fn open(path: &str) -> Result<Self, TestError> {
        let mut source: Vec<String> = vec![];
        let mut expected_output: Vec<String> = vec![];

        let fp = File::open(path).unwrap();
        let reader = BufReader::new(fp);
        let mut iter = reader.lines();

        while let Some(line) = iter.next() {
            let line = line.unwrap().trim().to_string();
            if line.starts_with("---") {
                break;
            }
            source.push(line);
        }

        while let Some(line) = iter.next() {
            expected_output.push(line.unwrap());
        }

        Ok(TestCase {
            name: path.to_string(),
            source,
            expected_output,
        })
    }

    fn print(&self) {
        println!("Test name: {}", self.name);
        println!("Source:");
        for ln in &self.source {
            println!("{}", ln);
        }
        println!("Expected output:");
        for ln in &self.expected_output {
            println!("{}", ln);
        }
    }

    fn evaluate(&self) -> Result<(), TestError> {
        let mut prg = Program::new();

        // Parse all lines in program
        for ln in &self.source {
            match parse_line(&ln) {
                Ok(line) => {
                    match prg.add(line) {
                        Ok(_) => {}
                        Err(e) => return Err(TestError::Program(e)),
                    };
                }
                Err(e) => match e {
                    ParserError::Lex(lex) => return Err(TestError::Lex(lex)),
                    ParserError::Syntax(syn) => return Err(TestError::Syntax(syn)),
                },
            }
        }

        // Run the program
        let run = RunStatement {};
        let mut ctx = Context::new();
        let mut sup = IntegrationSupport::new();

        match run.exec(&prg, &mut ctx, &mut sup) {
            Ok(_) => {}
            Err(RuntimeError::EndOfProgram) => {}
            Err(e) => return Err(TestError::Runtime(e)),
        };

        // Compare output with expectations
        assert_eq!(sup.output.len(), self.expected_output.len());
        for (output, expected) in sup.output.iter().zip(self.expected_output.iter()) {
            assert_eq!(output, expected);
        }

        println!("....... after exec .....");

        Ok(())
    }
}

fn run_one_test(path: &str) -> Result<(), TestError> {
    println!("Running test: {}", path);

    let tc = TestCase::open(path).unwrap();
    tc.print();
    tc.evaluate()
}

#[test]
fn find_better_name_for_this_test() -> Result<(), String> {
    for path in vec!["tests/hello.bas"] {
        match run_one_test("tests/hello.bas") {
            Ok(_) => {}
            Err(e) => return Err(format!("{}: {:?}", path, e)),
        }
    }

    Ok(())
}

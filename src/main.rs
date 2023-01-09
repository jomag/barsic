pub mod keyword;
pub mod lex;
pub mod parser;
pub mod shell;
pub mod statements;
pub mod support;

fn main() {
    let term = shell::Terminal {};
    shell::shell(&term);
}

pub mod keyword;
pub mod lex;
pub mod parser;
pub mod shell;
pub mod statements;

fn main() {
    let term = shell::Terminal {};
    shell::shell(&term);
}

pub mod keyword;
pub mod lex;
pub mod parser;
pub mod shell;
fn main() {
    let term = shell::Terminal {};
    shell::shell(&term);
}

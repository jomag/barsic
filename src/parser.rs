use crate::lex::{tokenize, LexError, Token};

pub enum Statement {}

pub struct Line {
    // The original source of this line
    pub source: String,

    // The line number
    pub num: Option<usize>,

    // The statements of this line
    pub statements: Vec<Statement>,
}

pub fn parse_statements(_tokens: &Vec<Token>) -> Result<Vec<Statement>, LexError> {
    println!("TODO: parse_statements");
    Ok(vec![])
}

pub fn parse_line(source: &str) -> Result<Line, LexError> {
    let mut tokens = tokenize(source)?;

    let num = match tokens[0] {
        Token::Int(n) => {
            tokens.remove(0);
            Some(n as usize)
        }
        _ => None,
    };

    let statements = parse_statements(&tokens)?;

    let line = Line {
        source: source.to_string(),
        num,
        statements,
    };

    return Ok(line);
}

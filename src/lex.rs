use crate::keyword::{DataType, Keyword};
use std::{
    error::Error,
    fmt,
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

#[derive(Debug)]
pub enum LexError {
    Eoi,
    InvalidFloat(String, ParseFloatError),
    InvalidInt(String, ParseIntError),
    InvalidToken(String),
    InternalError(String),
    UnterminatedString(String),
}

impl Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::Eoi => write!(f, "End of input"),
            LexError::InvalidFloat(s, e) => write!(f, "Invalid number (float): {} ({})", e, s),
            LexError::InvalidInt(s, e) => write!(f, "Invalid number (integer): {} ({})", e, s),
            LexError::InvalidToken(s) => write!(f, "Invalid token: {}", s),
            LexError::InternalError(s) => write!(f, "Internal error: {}", s),
            LexError::UnterminatedString(s) => write!(f, "Unterminated string: {}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Float(f64),
    Int(i64),
    Remark(String),
    Identifier(String),
    And,
    Or,
    Xor,
    Not,
    Keyword(Keyword),
    DataType(DataType),
    String(String),
    NE,
    LE,
    LT,
    GE,
    GT,
    Comma,
    LPar,
    RPar,
    Separator,
    Plus,
    Minus,
    Mul,
    Div,
    Colon,
    Semicolon,
    Underscore,
    Hash,
    ApproxEq,
    Eq,
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,

    Eq,
    GE,
    GT,
    LE,
    LT,
    NE,
    Not,
    And,
    Or,
    Xor,

    LPar,
    RPar,
    Separator,

    UMinus,
    UPlus,
    Call,
    Exp,
    StringEq,
    Concat,
    EQV,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::UMinus | Operator::Not => true,
            _ => false,
        }
    }
}

impl Token {
    pub fn is_operand(&self) -> bool {
        match self {
            Token::Int(_) | Token::Float(_) | Token::String(_) | Token::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn as_operator(&self) -> Option<Operator> {
        match self {
            Token::Mul => Some(Operator::Mul),
            Token::Div => Some(Operator::Div),
            Token::Plus => Some(Operator::Plus),
            Token::Minus => Some(Operator::Minus),
            Token::Eq => Some(Operator::Eq),
            Token::GE => Some(Operator::GE),
            Token::GT => Some(Operator::GT),
            Token::LE => Some(Operator::LE),
            Token::LT => Some(Operator::LT),
            Token::NE => Some(Operator::NE),
            Token::Not => Some(Operator::Not),
            Token::And => Some(Operator::And),
            Token::Or => Some(Operator::Or),
            Token::Xor => Some(Operator::Xor),
            Token::LPar => Some(Operator::LPar),
            Token::RPar => Some(Operator::RPar),
            Token::Comma => Some(Operator::Separator),
            _ => None,
        }
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let chars: Vec<char> = src.chars().collect();

    // It would be faster to hard code these
    let numeric = ('0'..='9').into_iter().collect::<String>();
    let lower = ('a'..='z').into_iter().collect::<String>();
    let upper = lower.to_uppercase().to_string();
    let alpha = format!("{}{}", lower, upper);

    // Legal characters for identifiers, except the first character
    // which must be alphabetic (uppercase or lowercase)
    let identifier = format!("{}{}%$", alpha, numeric);

    let mut tokens: Vec<Token> = vec![];
    let mut i: usize = 0;

    let parse_number = |i: &mut usize| -> Result<Token, LexError> {
        let start = *i;
        let mut dot_seen = false;

        while *i < chars.len() {
            match chars[*i] {
                c if c.is_numeric() => *i += 1,
                '.' => {
                    if dot_seen {
                        break;
                    } else {
                        dot_seen = true;
                        *i += 1;
                    }
                }
                _ => break,
            }
        }

        if start == *i {
            return Err(LexError::Eoi);
        }

        let sub: String = chars[start..*i].into_iter().collect();

        if dot_seen {
            return match sub.parse::<f64>() {
                Ok(f) => Ok(Token::Float(f)),
                Err(e) => Err(LexError::InvalidFloat(sub, e)),
            };
        }

        return match sub.parse::<i64>() {
            Ok(f) => Ok(Token::Int(f)),
            Err(e) => Err(LexError::InvalidInt(sub, e)),
        };
    };

    let skip_spaces = |i: &mut usize| {
        while *i < chars.len() {
            match chars[*i] {
                ' ' | '\t' => *i += 1,
                _ => break,
            }
        }
    };

    // Note! parse_remark() expects "i" to point at the character *after*
    // the initial remark character!
    let parse_remark = |i: &mut usize| -> Result<Token, LexError> {
        skip_spaces(i);
        let start = *i;
        *i = chars.len();

        Ok(Token::Remark(
            chars[start..].into_iter().collect::<String>(),
        ))
    };

    let parse_word = |i: &mut usize| -> Result<Token, LexError> {
        if !alpha.contains(chars[*i]) {
            return Err(LexError::InternalError(
                "First character of identifiers must be alphabetic".to_string(),
            ));
        }

        let start = *i;
        while *i < chars.len() && identifier.contains(chars[*i]) {
            *i += 1;
        }

        let word = chars[start..*i].into_iter().collect::<String>();

        match word.to_lowercase().as_str() {
            "rem" => {
                *i += 3;
                parse_remark(i)
            }
            "and" => Ok(Token::And),
            "or" => Ok(Token::Or),
            "xor" => Ok(Token::Xor),
            "not" => Ok(Token::Not),

            w => match Keyword::from_str(w) {
                Ok(kw) => Ok(Token::Keyword(kw)),
                Err(_) => match DataType::from_str(w) {
                    Ok(dt) => Ok(Token::DataType(dt)),
                    Err(_) => Ok(Token::Identifier(word)),
                },
            },
        }
    };

    let parse_string = |i: &mut usize| -> Result<Token, LexError> {
        let start = *i;
        *i += 1;
        while *i < chars.len() && chars[*i] != '"' {
            *i += 1;
        }

        if *i == chars.len() {
            let s = chars[start..*i].into_iter().collect::<String>();
            return Err(LexError::UnterminatedString(s));
        }

        let s = chars[(start + 1)..*i].into_iter().collect::<String>();
        *i += 1;
        Ok(Token::String(s))
    };

    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' => i += 1,
            c if c.is_numeric() => match parse_number(&mut i) {
                Ok(n) => tokens.push(n),
                Err(e) => return Err(e),
            },
            c if identifier.contains(c) => match parse_word(&mut i) {
                Ok(w) => tokens.push(w),
                Err(e) => return Err(e),
            },
            '"' => match parse_string(&mut i) {
                Ok(s) => tokens.push(s),
                Err(e) => return Err(e),
            },

            '\'' => {
                i += 1;
                match parse_remark(&mut i) {
                    Ok(rem) => tokens.push(rem),
                    Err(e) => return Err(e),
                }
            }

            ',' => {
                i += 1;
                tokens.push(Token::Comma)
            }
            '(' => {
                i += 1;
                tokens.push(Token::LPar)
            }
            ')' => {
                i += 1;
                tokens.push(Token::RPar)
            }
            '\\' => {
                i += 1;
                tokens.push(Token::Separator)
            }
            '+' => {
                i += 1;
                tokens.push(Token::Plus)
            }
            '-' => {
                i += 1;
                tokens.push(Token::Minus)
            }
            '*' => {
                i += 1;
                tokens.push(Token::Mul)
            }
            '/' => {
                i += 1;
                tokens.push(Token::Div)
            }
            ':' => {
                i += 1;
                tokens.push(Token::Colon)
            }
            ';' => {
                i += 1;
                tokens.push(Token::Semicolon)
            }
            '_' => {
                i += 1;
                tokens.push(Token::Underscore)
            }
            '#' => {
                i += 1;
                tokens.push(Token::Hash)
            }

            '<' => {
                let nxt = if i + 1 < chars.len() {
                    Some(chars[i + 1])
                } else {
                    None
                };
                match nxt {
                    Some('>') => {
                        i += 2;
                        tokens.push(Token::NE)
                    }
                    Some('=') => {
                        i += 2;
                        tokens.push(Token::LE)
                    }
                    Some(_) | None => {
                        i += 1;
                        tokens.push(Token::LT)
                    }
                };
            }

            '>' => {
                let nxt = if i + 1 < chars.len() {
                    Some(chars[i + 1])
                } else {
                    None
                };
                match nxt {
                    Some('=') => {
                        i += 2;
                        tokens.push(Token::GE)
                    }
                    Some(_) | None => {
                        i += 1;
                        tokens.push(Token::GT)
                    }
                };
            }

            '=' => {
                let nxt = if i + 1 < chars.len() {
                    Some(chars[i + 1])
                } else {
                    None
                };
                match nxt {
                    Some('=') => {
                        i += 2;
                        tokens.push(Token::ApproxEq)
                    }
                    Some(_) | None => {
                        i += 1;
                        tokens.push(Token::Eq)
                    }
                };
            }

            _ => {
                return Err(LexError::InvalidToken(
                    chars[i..].into_iter().collect::<String>(),
                ))
            }
        }
    }

    Ok(tokens)
}

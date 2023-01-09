use std::{collections::VecDeque, rc::Rc};

use crate::{
    keyword::Keyword,
    lex::{tokenize, LexError, Operator, Token},
    statements::run_statement::{Context, Program},
    support::Support,
};

#[derive(Debug)]
pub enum SyntaxError {
    IllegalStatement(String),
    Internal(String),
}

#[derive(Debug)]
pub enum ParserError {
    Lex(LexError),
    Syntax(SyntaxError),
}

// Expression evaluation error
#[derive(Debug)]
pub enum EvaluationError {}

#[derive(Debug)]
pub enum RuntimeError {
    Evaluation(EvaluationError),
    EndOfProgram,
}

pub trait Statement: core::fmt::Debug {
    fn exec(
        &self,
        prg: &Program,
        ctx: &mut Context,
        sup: &mut dyn Support,
    ) -> Result<usize, RuntimeError>;
}

#[derive(Clone, Debug)]
pub enum FlowStatement {
    EndOfLine,
}

impl Statement for FlowStatement {
    fn exec(
        &self,
        prg: &Program,
        ctx: &mut Context,
        sup: &mut dyn Support,
    ) -> Result<usize, RuntimeError> {
        Ok(ctx.pc + 1)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Ident(String),
    Relational(Operator, Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    UnaryMinusExpr(Box<Expr>),
}

pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
}

impl Expr {
    pub fn evaluate(&self, ctx: &Context) -> Result<Value, EvaluationError> {
        match self {
            Expr::Int(_) => todo!(),
            Expr::Float(_) => todo!(),
            Expr::String(v) => Ok(Value::String(v.clone())),
            Expr::Ident(_) => todo!(),
            Expr::Relational(_, _, _) => todo!(),
            Expr::Add(_, _) => todo!(),
            Expr::Sub(_, _) => todo!(),
            Expr::Mul(_, _) => todo!(),
            Expr::Div(_, _) => todo!(),
            Expr::And(_, _) => todo!(),
            Expr::Or(_, _) => todo!(),
            Expr::Xor(_, _) => todo!(),
            Expr::Not(_) => todo!(),
            Expr::UnaryMinusExpr(_) => todo!(),
        }
    }
}

fn left_assoc(op: Operator) -> bool {
    match op {
        Operator::Mul | Operator::Div => true,
        Operator::Plus | Operator::Minus => true,
        Operator::LE | Operator::LT => true,
        Operator::GE | Operator::GT => true,
        Operator::Eq | Operator::NE => true,
        Operator::And | Operator::Or | Operator::Xor => true,
        _ => false,
    }
}

fn prec(op: Operator) -> i32 {
    match op {
        Operator::Exp => 1,
        Operator::UMinus | Operator::UPlus => 2,
        Operator::Mul | Operator::Div => 3,
        Operator::Plus | Operator::Minus => 4,
        Operator::Concat => 5,
        Operator::Eq
        | Operator::GE
        | Operator::GT
        | Operator::LE
        | Operator::LT
        | Operator::NE
        | Operator::StringEq => 6,
        Operator::Not => 7,
        Operator::And => 8,
        Operator::Or => 9,
        Operator::Xor => 10,
        Operator::EQV => 11,
        Operator::LPar | Operator::RPar | Operator::Separator | Operator::Call => {
            panic!("should never happen")
        }
    }
}

#[derive(Clone, Debug)]
enum PrintItem {
    // Item is printed immediately after the previous item
    Consecutive(Expr),

    // Item is printed in the next "zone" (tab)
    Zoned(Expr),
}

// A few details about how values are formatted:
//
// Grouping:
//
// Values separated by comma (PRINT 1, 2, 3) should be grouped into
// columns of 14 characters:
//
// PRINT "ABC", "DEF", "GHI"  =>  'ABC           DEF           GHI'
//
// If one value spans more than 14 characters, the following values are
// moved further so that they start on a column evenly divisible by 14.
//
// Values separated by semicolon are not separated by any whitespace:
//
// PRINT "ABC", "DEF", "GHI" => "ABCDEFGHI"
//
// Linebreaks:
//
// A linebreak is always inserted at the end of the print statement unless
// the statement ends with a semicolon or comma.
//
// Numbers:
//
// Numbers are either preceded with a minus sign or space character, depending
// on if it's a negative or positive value. They are superseded by a space character.
//
// PRINT 1; -1; 123;"Done"  =>  ' 1 -1  123 Done'
#[derive(Debug)]
pub struct PrintStatement {
    channel: Option<Expr>,
    values: Vec<PrintItem>,
}

impl Clone for PrintStatement {
    fn clone(&self) -> Self {
        Self {
            channel: self.channel.clone(),
            values: self.values.clone(),
        }
    }
}

impl Statement for PrintStatement {
    fn exec(
        &self,
        _prg: &Program,
        ctx: &mut Context,
        sup: &mut dyn Support,
    ) -> Result<usize, RuntimeError> {
        let format = |v: Value| -> String {
            match v {
                Value::Int(i) => {
                    if i < 0 {
                        format!("{} ", i)
                    } else {
                        format!(" {} ", i)
                    }
                }
                Value::Float(f) => {
                    if f < 0.0 {
                        format!("{} ", f)
                    } else {
                        format!(" {} ", f)
                    }
                }
                Value::String(s) => s.clone(),
            }
        };

        let mut output: String = "".into();

        // FIXME: handle channels properly
        let channel: usize = 123;

        let mut iter = self.values.iter();
        while let Some(pi) = iter.next() {
            match pi {
                PrintItem::Consecutive(expr) | PrintItem::Zoned(expr) => {
                    let result = match expr.evaluate(ctx) {
                        Ok(v) => v,
                        Err(e) => return Err(RuntimeError::Evaluation(e)),
                    };
                    match pi {
                        PrintItem::Consecutive(_) => {
                            output = format!("{}{}", output, format(result))
                        }
                        PrintItem::Zoned(_) => output = format!("{}\t{}", output, format(result)),
                    }
                }
            }
        }

        // FIXME: grouped/zoned functionality is not correctly implemented (only uses \t). See Bajsic equivalent.
        // FIXME: skipping linebreak is not implemented. Add another PrintItem ("LineBreak") and handle
        // it in the parser.
        sup.print(channel, &output);
        Ok(ctx.pc + 1)
    }
}

#[derive(Debug)]
pub struct Line {
    // The original source of this line
    pub source: String,

    // The line number
    pub num: Option<usize>,

    // The statements of this line
    pub statements: Vec<Rc<dyn Statement>>,
}

pub fn parse_statements(tokens: &Vec<Token>) -> Result<Vec<Rc<dyn Statement>>, SyntaxError> {
    let mut statements: Vec<Rc<dyn Statement>> = vec![];

    // FIXME: For some reason, VecDeque::from(tokens) did not compile. Investigate.
    let mut token_deque: VecDeque<&Token> = VecDeque::new();
    for t in tokens {
        token_deque.push_back(t);
    }

    while !token_deque.is_empty() {
        statements.push(parse_statement(&mut token_deque)?);

        // Drop next token if it is a separator token
        if let Some(tkn) = token_deque.front() {
            if let Token::Separator = tkn {
                token_deque.pop_front();
            }
        }
    }

    Ok(statements)
}

macro_rules! must_pop_keyword {
    ($tokens:expr, $expected_keyword:path) => {
        match $tokens.pop_front() {
            Some(Token::Keyword(kw)) => match kw {
                $expected_keyword => {}
                kw => {
                    return Err(SyntaxError::Internal(format!(
                        "Expected keyword {:?}, got {:?}",
                        $expected_keyword, kw,
                    )))
                }
            },
            Some(tkn) => {
                return Err(SyntaxError::Internal(format!(
                    "Expected keyword token {:?}, got {:?}",
                    $expected_keyword, tkn,
                )))
            }
            None => {
                return Err(SyntaxError::Internal(format!(
                    "Expected keyword {:?}, got end-of-input",
                    $expected_keyword,
                )))
            }
        }
    };
}

macro_rules! pop_optional {
    ($tokens:expr, $tkn:path) => {
        match $tokens.front() {
            Some($tkn) => {
                $tokens.pop_front();
                true
            }
            Some(_) => false,
            None => false,
        }
    };
}

fn parse_expression(tokens: &mut VecDeque<&Token>) -> Result<Expr, SyntaxError> {
    let mut operator_stack = VecDeque::<Operator>::new();
    let mut expr_stack = VecDeque::<Expr>::new();
    let mut arg_count_stack = VecDeque::<usize>::new();
    let mut empty_bracket_stack = VecDeque::<bool>::new();
    let mut possible_function_call = false;

    let set_not_empty_bracket = |empty_bracket_stack: &mut VecDeque<bool>| {
        match empty_bracket_stack.back() {
            Some(true) => {
                empty_bracket_stack.pop_back();
                empty_bracket_stack.push_back(false);
            }
            _ => {}
        };
    };

    let not_open_bracket = |operator_stack: &VecDeque<Operator>| match operator_stack.back() {
        Some(Operator::LPar) | Some(Operator::Call) => false,
        Some(_) => true,
        None => panic!("fixme: i think this is an error..."),
    };

    let build_unary_operator_expr = |operator: Operator, operand: Expr| match operator {
        Operator::Not => Expr::Not(operand.into()),
        Operator::UMinus => Expr::UnaryMinusExpr(operand.into()),
        _ => panic!("Can't build unary operator of type {:?}", operator),
    };

    let build_binary_operator_expr = |operator: Operator, ch1: Expr, ch2: Expr| match operator {
        Operator::LT | Operator::LE | Operator::GT | Operator::GE | Operator::Eq | Operator::NE => {
            Expr::Relational(operator, ch1.into(), ch2.into())
        }
        Operator::Plus => Expr::Add(ch1.into(), ch2.into()),
        Operator::Minus => Expr::Sub(ch1.into(), ch2.into()),
        Operator::Mul => Expr::Mul(ch1.into(), ch2.into()),
        Operator::Div => Expr::Div(ch1.into(), ch2.into()),
        Operator::And => Expr::And(ch1.into(), ch2.into()),
        Operator::Or => Expr::Or(ch1.into(), ch2.into()),
        Operator::Xor => Expr::Xor(ch1.into(), ch2.into()),
        _ => panic!("not unary"),
    };

    let build_expr = |expr_stack: &mut VecDeque<Expr>, op: Operator| {
        if op.is_unary() {
            let operand = expr_stack.pop_back().unwrap();
            build_unary_operator_expr(op, operand)
        } else {
            let op1 = expr_stack.pop_back().unwrap();
            let op2 = expr_stack.pop_back().unwrap();
            build_binary_operator_expr(op, op1.into(), op2.into())
        }
    };

    while let Some(tok) = tokens.front() {
        let op = match tok.as_operator() {
            Some(Operator::Minus) if !possible_function_call => Some(Operator::UMinus),
            o => o,
        };

        // Determine end of expression
        match op {
            Some(Operator::Separator) => {
                if arg_count_stack.is_empty() {
                    break;
                }
            }
            Some(Operator::LPar) => {
                empty_bracket_stack.push_back(true);
                if possible_function_call {
                    operator_stack.push_back(Operator::Call);
                    arg_count_stack.push_back(0);
                } else {
                    operator_stack.push_back(Operator::LPar);
                }
                possible_function_call = false;
            }
            Some(Operator::RPar) => {
                // FIXME: verify how this works. Seems like it should be an error if stack is empty.
                let empty = empty_bracket_stack.pop_back().unwrap();

                while not_open_bracket(&operator_stack) {
                    let op = operator_stack.pop_back().unwrap();
                    let expr = build_expr(&mut expr_stack, op);
                    expr_stack.push_back(expr);
                }

                match operator_stack.back() {
                    Some(Operator::Call) => {
                        todo!();
                        // let arg_count =
                        //     arg_count_stack.pop_back().unwrap() + (if empty { 0 } else { 1 });
                        // let args = [];
                        // while arg_count > 0 {
                        //     args = [...args, expr_stack.pop_back()];
                        //     arg_count -= 1;
                        // }
                        // let fun = expr_stack.pop_back();
                        // let call = Expr::Call(fun, args.reverse());
                        // expr_stack.push(call);
                    }
                    None => {
                        // No matching left paranthesis in expression
                        // The closing paranthesis is likely part of the
                        // outer statement, so assume we've reached the end
                        // of the expression.
                        break;
                    }
                    _ => {}
                }

                possible_function_call = true;
                operator_stack.pop_back();
            }

            Some(Operator::Separator) => {
                while not_open_bracket(&operator_stack) {
                    let expr = build_expr(&mut expr_stack, operator_stack.pop_back().unwrap());
                    expr_stack.push_back(expr);
                }

                match arg_count_stack.pop_back() {
                    Some(n) => arg_count_stack.push_back(n + 1),
                    None => arg_count_stack.push_back(1),
                };

                possible_function_call = false;
            }

            Some(op) => {
                // Only binary operators remaining
                while !operator_stack.is_empty() {
                    let o = match operator_stack.back() {
                        Some(o) => o,
                        None => panic!("fixme: i don't think this should ever happen"),
                    };

                    match o {
                        Operator::LPar => break,
                        _ => {
                            let c1 = prec(*o) < prec(op);
                            let c2 = prec(*o) == prec(op) && left_assoc(*o);
                            if c1 || c2 {
                                operator_stack.pop_back();
                            }
                        }
                    }
                }

                possible_function_call = false;
                operator_stack.push_back(op);
            }

            None => {
                if tok.is_operand() {
                    match build_operand_expression(tok) {
                        Ok(expr) => expr_stack.push_back(expr.into()),
                        Err(e) => return Err(e),
                    };
                    set_not_empty_bracket(&mut empty_bracket_stack);
                    possible_function_call = true;
                } else {
                    // Neither an operand or an operator. End of expression.
                    break;
                }
            }
        }

        tokens.pop_front();
    }

    while !operator_stack.is_empty() {
        let op = operator_stack.pop_back().unwrap();
        let expr = build_expr(&mut expr_stack, op);
        expr_stack.push_back(expr);
    }

    assert!(operator_stack.is_empty());
    return Ok(expr_stack.pop_back().unwrap());
}

fn build_operand_expression(tok: &Token) -> Result<Expr, SyntaxError> {
    match tok {
        Token::Int(value) => Ok(Expr::Int(*value)),
        Token::Float(value) => Ok(Expr::Float(*value)),
        Token::String(value) => Ok(Expr::String(value.clone())),
        Token::Identifier(name) => Ok(Expr::Ident(name.clone())),
        _ => Err(SyntaxError::Internal(format!(
            "Can't build operand expression from {:?}",
            tok,
        ))),
    }
}

fn parse_print_statement(tokens: &mut VecDeque<&Token>) -> Result<Rc<PrintStatement>, SyntaxError> {
    // VAX BASIC Ref: page 462
    // Format: PRINT, PRINT expr, PRINT #channel, expr
    must_pop_keyword!(tokens, Keyword::PRINT);
    let mut channel: Option<Expr> = None;

    if pop_optional!(tokens, Token::Hash) {
        channel = Some(parse_expression(tokens)?);
        if !pop_optional!(tokens, Token::Comma) {
            return Ok(PrintStatement {
                channel,
                values: vec![],
            }
            .into());
        }
    }

    let mut list: Vec<PrintItem> = vec![];
    let mut delim = Token::Comma;

    while !tokens.is_empty() {
        let expr = parse_expression(tokens)?;
        list.push(match delim {
            Token::Comma => PrintItem::Consecutive(expr),
            Token::Semicolon => PrintItem::Zoned(expr),
            _ => return Err(SyntaxError::Internal("Invalid PRINT delimiter".into())),
        });

        match tokens.front() {
            Some(Token::Comma) => delim = Token::Comma,
            Some(Token::Semicolon) => delim = Token::Semicolon,
            _ => break,
        }
    }

    Ok(PrintStatement {
        channel,
        values: list,
    }
    .into())
}

fn parse_primary_statement(
    tokens: &mut VecDeque<&Token>,
) -> Result<Rc<impl Statement>, SyntaxError> {
    match tokens.front().unwrap() {
        Token::Keyword(kw) => match kw {
            Keyword::PRINT => parse_print_statement(tokens),
            _ => Err(SyntaxError::IllegalStatement(format!(
                "Illegal syntax. First token is keyword {:?}",
                *kw
            ))),
        },
        tkn => Err(SyntaxError::IllegalStatement(format!(
            "Illegal syntax. First token is {:?}",
            tkn
        ))),
    }
}

fn parse_statement(tokens: &mut VecDeque<&Token>) -> Result<Rc<impl Statement>, SyntaxError> {
    parse_primary_statement(tokens)
}

pub fn parse_line(source: &str) -> Result<Line, ParserError> {
    let mut tokens = match tokenize(source) {
        Ok(t) => t,
        Err(e) => return Err(ParserError::Lex(e)),
    };

    let num = match tokens[0] {
        Token::Int(n) => {
            tokens.remove(0);
            Some(n as usize)
        }
        _ => None,
    };

    let statements = match parse_statements(&tokens) {
        Ok(t) => t,
        Err(e) => return Err(ParserError::Syntax(e)),
    };

    let line = Line {
        source: source.to_string(),
        num,
        statements,
    };

    return Ok(line);
}

use std::{fmt, error::Error};

use crate::lexer::Tokenizer;

pub enum Expr {
    Number(f64),
    Variable(String),
    Binary {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Box<Expr>>,
    },
}

pub struct Prototype {
    name: String,
    args: Vec<String>,
}

pub struct Function {
    proto: Box<Prototype>,
    body: Box<Expr>,
}

// pub fn parse_expr(tok: Tokenizer) -> Box<Expr> {
    
// }

// fn parse_number() -> Box<Expr> {
//     Box::new()
// }

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for ParseError {}

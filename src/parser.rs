use std::{collections::HashMap, error::Error, fmt, sync::OnceLock};

use crate::lexer::{LexError, Token, TokenKind, TokenPosition, TokenStream};

#[derive(Debug)]
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
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub params: Vec<String>,
}

#[derive(Debug)]
pub struct Function {
    pub proto: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

macro_rules! pop_token_expect {
    ( $tokens:ident : $kind:pat => $ret:expr ) => {
        match $tokens.pop() {
            Ok(Token {
                kind: $kind,
                pos: _,
            }) => $ret,
            Ok(Token { kind: k, pos: p }) => {
                return Err(UnexpectedTokenError { kind: k, pos: p }.into())
            }
            Err(e) => return Err(ParseError::Lex(e)),
        }
    };
}

const ANON_FUNC_NAME: &str = "__anon";

static OP_PRECEDENCE: OnceLock<HashMap<String, i32>> = OnceLock::new();

fn get_op_precedence(op: &String) -> Result<i32> {
    match OP_PRECEDENCE
        .get_or_init(|| {
            HashMap::from([
                ("<".into(), 10),
                (">".into(), 10),
                ("+".into(), 20),
                ("-".into(), 20),
                ("*".into(), 30),
                ("/".into(), 30),
            ])
        })
        .get(op)
    {
        Some(&p) => Ok(p),
        _ => Err(ParseError::Other(format!("unknown operator '{op}'"))),
    }
}

pub fn parse(tokens: &mut TokenStream) -> Result<Vec<Function>> {
    let mut ast_nodes: Vec<Function> = Vec::new();
    loop {
        match tokens.peek_kind() {
            TokenKind::Eof => {
                break;
            }
            TokenKind::Semicolon => {
                tokens.pop()?;
                continue;
            }
            TokenKind::Def => {
                let func = parse_definition(tokens)?;
                ast_nodes.push(func);
            }
            TokenKind::Extern => {
                let ext = parse_extern(tokens)?;
                ast_nodes.push(ext);
            }
            _ => {
                let top = parse_toplevel_expr(tokens)?;
                ast_nodes.push(top);
            }
        }
    }
    Ok(ast_nodes)
}

fn parse_definition(tokens: &mut TokenStream) -> Result<Function> {
    pop_token_expect!(tokens : TokenKind::Def => ());

    let proto = parse_prototype(tokens)?;
    let body = parse_expr(tokens)?;
    Ok(Function {
        proto,
        body: Some(body),
        is_anon: false,
    })
}

fn parse_extern(tokens: &mut TokenStream) -> Result<Function> {
    pop_token_expect!(tokens : TokenKind::Extern => ());

    let proto = parse_prototype(tokens)?;
    Ok(Function { proto, body: None, is_anon: false })
}

fn parse_prototype(tokens: &mut TokenStream) -> Result<Prototype> {
    let name = pop_token_expect!(tokens : TokenKind::Identifier(id) => id);

    pop_token_expect!(tokens : TokenKind::LParen => ());

    let mut params: Vec<String> = Vec::new();
    loop {
        match tokens.peek_kind() {
            TokenKind::RParen => break,
            TokenKind::Identifier(p) => {
                params.push(p.clone());
                tokens.pop()?;
            }
            k => {
                return Err(UnexpectedTokenError {
                    kind: k.clone(),
                    pos: tokens.peek_pos().clone(),
                }
                .into())
            }
        }
        match tokens.peek_kind() {
            TokenKind::RParen => break,
            TokenKind::Comma => {
                tokens.pop()?;
            }
            k => {
                return Err(UnexpectedTokenError {
                    kind: k.clone(),
                    pos: tokens.peek_pos().clone(),
                }
                .into())
            }
        }
    }

    pop_token_expect!(tokens : TokenKind::RParen => ());

    Ok(Prototype { name, params })
}

fn parse_toplevel_expr(tokens: &mut TokenStream) -> Result<Function> {
    let proto = Prototype {
        name: ANON_FUNC_NAME.into(),
        params: Vec::new(),
    };
    let body = parse_expr(tokens)?;
    Ok(Function {
        proto,
        body: Some(body),
        is_anon: true,
    })
}

fn parse_expr(tokens: &mut TokenStream) -> Result<Expr> {
    let lhs = parse_primary_expr(tokens)?;
    parse_binary_expr_rhs(tokens, 0, lhs)
}

fn parse_primary_expr(tokens: &mut TokenStream) -> Result<Expr> {
    match tokens.peek_kind() {
        TokenKind::Identifier(_) => parse_identifier(tokens),
        TokenKind::Number(_) => parse_number(tokens),
        TokenKind::LParen => parse_paren_expr(tokens),
        k => {
            return Err(UnexpectedTokenError {
                kind: k.clone(),
                pos: tokens.peek_pos().clone(),
            }
            .into())
        }
    }
}

fn parse_identifier(tokens: &mut TokenStream) -> Result<Expr> {
    let id = pop_token_expect!(tokens : TokenKind::Identifier(id) => id);

    // Variable reference
    match tokens.peek_kind() {
        TokenKind::LParen => (),
        _ => return Ok(Expr::Variable(id)),
    }

    // Call
    tokens.pop()?; // LParen
    let mut args: Vec<Expr> = Vec::new();
    loop {
        match tokens.peek_kind() {
            TokenKind::RParen => break,
            _ => {
                args.push(parse_expr(tokens)?);
            }
        }
        match tokens.peek_kind() {
            TokenKind::RParen => break,
            TokenKind::Comma => {
                tokens.pop()?;
            }
            k => {
                return Err(UnexpectedTokenError {
                    kind: k.clone(),
                    pos: tokens.peek_pos().clone(),
                }
                .into())
            }
        }
    }
    tokens.pop()?; // RParen
    Ok(Expr::Call { callee: id, args })
}

fn parse_number(tokens: &mut TokenStream) -> Result<Expr> {
    let n = pop_token_expect!(tokens : TokenKind::Number(n) => n);
    Ok(Expr::Number(n))
}

fn parse_paren_expr(tokens: &mut TokenStream) -> Result<Expr> {
    pop_token_expect!(tokens : TokenKind::LParen => ());
    let expr = parse_expr(tokens);
    pop_token_expect!(tokens : TokenKind::RParen => ());
    expr
}

fn parse_binary_expr_rhs(tokens: &mut TokenStream, expr_prec: i32, mut lhs: Expr) -> Result<Expr> {
    loop {
        let (op, op_prec) = if let TokenKind::Operator(op) = tokens.peek_kind() {
            let op_prec = get_op_precedence(op)?;
            if op_prec < expr_prec {
                return Ok(lhs);
            }
            (op.clone(), op_prec)
        } else {
            return Ok(lhs);
        };

        tokens.pop()?; // op
        let mut rhs = parse_primary_expr(tokens)?;
        if let TokenKind::Operator(next_op) = tokens.peek_kind() {
            let next_op_prec = get_op_precedence(next_op)?;
            if op_prec < next_op_prec {
                rhs = parse_binary_expr_rhs(tokens, op_prec + 1, rhs)?;
            }
        }

        lhs = Expr::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
    }
}

#[derive(Debug)]
pub struct UnexpectedTokenError {
    kind: TokenKind,
    pos: TokenPosition,
}

impl fmt::Display for UnexpectedTokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unexpected token {:?} at {}", self.kind, self.pos)
    }
}

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(UnexpectedTokenError),
    Other(String),
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::Lex(err)
    }
}

impl From<UnexpectedTokenError> for ParseError {
    fn from(err: UnexpectedTokenError) -> Self {
        ParseError::UnexpectedToken(err)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Other(s) => {
                write!(f, "{s}")
            }
            _ => {
                write!(f, "{}", self)
            }
        }
    }
}

impl Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

#[cfg(test)]
mod tests {
    use crate::{lexer::TokenStream, parser};

    #[test]
    fn parse_expr_binary_1() {
        let mut tokens = TokenStream::new("a + b * c").unwrap();
        let res = parser::parse_expr(&mut tokens).unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Variable(\"a\"), rhs: Binary { op: \"*\", lhs: Variable(\"b\"), rhs: Variable(\"c\") } }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_expr_binary_2() {
        let mut tokens = TokenStream::new("a * b + c").unwrap();
        let res = parser::parse_expr(&mut tokens).unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Binary { op: \"*\", lhs: Variable(\"a\"), rhs: Variable(\"b\") }, rhs: Variable(\"c\") }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_identifier_call() {
        let mut tokens = TokenStream::new("f(1, 2,)").unwrap();
        let res = parser::parse_identifier(&mut tokens).unwrap();
        assert_eq!(
            "Call { callee: \"f\", args: [Number(1.0), Number(2.0)] }",
            format!("{:?}", res),
        );
    }
}

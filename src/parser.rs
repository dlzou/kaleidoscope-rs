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
}

// macro_rules! pop_token_expect {
//     ( $tokens:ident => $kind:pat ) => {
//         match $tokens.pop() {
//             Ok(Token {
//                 kind: $kind,
//                 pos: _,
//             }) => (),
//             Ok(Token { kind: k, pos: p }) => {
//                 return Err(UnexpectedTokenError { kind: k, pos: p }.into())
//             }
//             Err(e) => return Err(ParseError::Lex(e)),
//         }
//     };
// }

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

pub fn parse(tokens: &mut Vec<TokenKind>) -> Result<Vec<Function>> {
    let mut ast_nodes: Vec<Function> = Vec::new();
    loop {
        match tokens.last() {
            Some(TokenKind::Eof) => {
                break;
            }
            Some(TokenKind::Semicolon) => {
                tokens.pop();
                continue;
            }
            Some(TokenKind::Def) => {
                let func = parse_definition(tokens)?;
                ast_nodes.push(func);
            }
            Some(TokenKind::Extern) => {
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

fn parse_definition(tokens: &mut Vec<TokenKind>) -> Result<Function> {
    match tokens.pop() {
        Some(TokenKind::Def) => (),
        _ => return Err(ParseError::Other("expected 'def'".into())),
    }

    let proto = parse_prototype(tokens)?;
    let body = parse_expr(tokens)?;
    Ok(Function {
        proto,
        body: Some(body),
    })
}

fn parse_extern(tokens: &mut Vec<TokenKind>) -> Result<Function> {
    match tokens.pop() {
        Some(TokenKind::Extern) => (),
        _ => return Err(ParseError::Other("expected 'extern'".into())),
    }

    let proto = parse_prototype(tokens)?;
    Ok(Function { proto, body: None })
}

fn parse_prototype(tokens: &mut Vec<TokenKind>) -> Result<Prototype> {
    let name = match tokens.pop() {
        Some(TokenKind::Identifier(id)) => id,
        _ => return Err(ParseError::Other("expected function name".into())),
    };

    match tokens.pop() {
        Some(TokenKind::LParen) => (),
        _ => return Err(ParseError::Other("expected '('".into())),
    }

    let mut params: Vec<String> = Vec::new();
    loop {
        match tokens.last() {
            Some(TokenKind::RParen) => break,
            Some(TokenKind::Identifier(p)) => {
                params.push(p.clone());
                tokens.pop();
            }
            _ => return Err(ParseError::Other("expected identifier or ')'".into())),
        }
        match tokens.last() {
            Some(&TokenKind::RParen) => break,
            Some(&TokenKind::Comma) => {
                tokens.pop();
            }
            _ => return Err(ParseError::Other("expected ')' or ','".into())),
        }
    }
    while let Some(TokenKind::Identifier(p)) = tokens.last() {
        params.push(p.clone());
        tokens.pop();
    }

    match tokens.pop() {
        Some(TokenKind::RParen) => (),
        _ => return Err(ParseError::Other("expected ')'".into())),
    }

    Ok(Prototype { name, params })
}

fn parse_toplevel_expr(tokens: &mut Vec<TokenKind>) -> Result<Function> {
    let proto = Prototype {
        name: "".into(),
        params: Vec::new(),
    };
    let body = parse_expr(tokens)?;
    Ok(Function {
        proto,
        body: Some(body),
    })
}

fn parse_expr(tokens: &mut Vec<TokenKind>) -> Result<Expr> {
    let lhs = parse_primary_expr(tokens)?;
    parse_binary_expr_rhs(tokens, 0, lhs)
}

fn parse_primary_expr(toks: &mut Vec<TokenKind>) -> Result<Expr> {
    match toks.last() {
        Some(&TokenKind::Identifier(_)) => parse_identifier(toks),
        Some(&TokenKind::Number(_)) => parse_number(toks),
        Some(&TokenKind::LParen) => parse_paren_expr(toks),
        Some(t) => Err(ParseError::Other(format!(
            "expected primary expression, got {:?}",
            *t
        ))),
        None => Err(ParseError::Other(
            "expected primary expression, got nothing".into(),
        )),
    }
}

fn parse_identifier(tokens: &mut Vec<TokenKind>) -> Result<Expr> {
    let id = match tokens.pop() {
        Some(TokenKind::Identifier(id)) => id,
        _ => return Err(ParseError::Other("expected identifier".into())),
    };

    // Variable reference
    match tokens.last() {
        Some(&TokenKind::LParen) => (),
        _ => return Ok(Expr::Variable(id)),
    }

    // Call
    tokens.pop(); // LParen
    let mut args: Vec<Expr> = Vec::new();
    loop {
        match tokens.last() {
            Some(TokenKind::RParen) => break,
            _ => {
                args.push(parse_expr(tokens)?);
            }
        }
        match tokens.last() {
            Some(&TokenKind::RParen) => break,
            Some(&TokenKind::Comma) => {
                tokens.pop();
            }
            _ => return Err(ParseError::Other("expected ')' or ','".into())),
        }
    }
    tokens.pop(); // RParen
    Ok(Expr::Call { callee: id, args })
}

fn parse_number(tokens: &mut Vec<TokenKind>) -> Result<Expr> {
    let n = match tokens.pop() {
        Some(TokenKind::Number(n)) => n,
        _ => return Err(ParseError::Other("expected number".into())),
    };
    Ok(Expr::Number(n))
}

fn parse_paren_expr(tokens: &mut Vec<TokenKind>) -> Result<Expr> {
    match tokens.pop() {
        Some(TokenKind::LParen) => (),
        _ => return Err(ParseError::Other("expected '('".into())),
    }

    let expr = parse_expr(tokens);
    match tokens.pop() {
        Some(TokenKind::RParen) => (),
        _ => return Err(ParseError::Other("expected ')'".into())),
    }
    expr
}

fn parse_binary_expr_rhs(
    tokens: &mut Vec<TokenKind>,
    expr_prec: i32,
    mut lhs: Expr,
) -> Result<Expr> {
    loop {
        let (op, op_prec) = if let Some(TokenKind::Operator(op)) = tokens.last() {
            let op_prec = get_op_precedence(op)?;
            if op_prec < expr_prec {
                return Ok(lhs);
            }
            (op.clone(), op_prec)
        } else {
            return Ok(lhs);
        };

        tokens.pop(); // op
        let mut rhs = parse_primary_expr(tokens)?;
        if let Some(TokenKind::Operator(next_op)) = tokens.last() {
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
    use crate::{lexer::TokenKind, parser};

    #[test]
    fn parse_expr_binary_1() {
        let mut tokens = vec![
            TokenKind::Identifier("a".into()),
            TokenKind::Operator("+".into()),
            TokenKind::Identifier("b".into()),
            TokenKind::Operator("*".into()),
            TokenKind::Identifier("c".into()),
        ];
        tokens.reverse();
        let res = parser::parse_expr(&mut tokens).unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Variable(\"a\"), rhs: Binary { op: \"*\", lhs: Variable(\"b\"), rhs: Variable(\"c\") } }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_expr_binary_2() {
        let mut tokens = vec![
            TokenKind::Identifier("a".into()),
            TokenKind::Operator("*".into()),
            TokenKind::Identifier("b".into()),
            TokenKind::Operator("+".into()),
            TokenKind::Identifier("c".into()),
        ];
        tokens.reverse();
        let res = parser::parse_expr(&mut tokens).unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Binary { op: \"*\", lhs: Variable(\"a\"), rhs: Variable(\"b\") }, rhs: Variable(\"c\") }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_identifier_call() {
        let mut tokens = vec![
            TokenKind::Identifier("f".into()),
            TokenKind::LParen,
            TokenKind::Number(1.0),
            TokenKind::Comma,
            TokenKind::Number(2.0),
            TokenKind::Comma,
            TokenKind::RParen,
        ];
        tokens.reverse();
        let res = parser::parse_identifier(&mut tokens).unwrap();
        assert_eq!(
            "Call { callee: \"f\", args: [Number(1.0), Number(2.0)] }",
            format!("{:?}", res),
        );
    }
}

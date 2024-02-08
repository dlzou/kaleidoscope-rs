use std::{collections::HashMap, error::Error, fmt, sync::OnceLock};

use crate::lexer::Token;

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
        args: Vec<Box<Expr>>,
    },
}

#[derive(Debug)]
pub struct Prototype {
    name: String,
    args: Vec<String>,
}

#[derive(Debug)]
pub struct Function {
    proto: Box<Prototype>,
    body: Box<Expr>,
}

#[derive(Debug)]
pub enum ASTNode {
    FunctionNode(Function),
    ExternNode(Prototype),
}

static OP_PRECEDENCE: OnceLock<HashMap<String, i32>> = OnceLock::new();

fn get_tok_precedence(op: &String) -> Result<i32> {
    match OP_PRECEDENCE
        .get_or_init(|| {
            HashMap::from([
                ("<".into(), 1),
                (">".into(), 1),
                ("+".into(), 2),
                ("-".into(), 2),
                ("*".into(), 3),
                ("/".into(), 3),
            ])
        })
        .get(op)
    {
        Some(&p) => Ok(p),
        _ => Err(ParseError(format!("unknown operator '{op}'"))),
    }
}

pub fn parse(toks: &mut Vec<Token>) -> Result<Vec<ASTNode>> {
    let mut ast_nodes: Vec<ASTNode> = Vec::new();
    loop {
        match toks.last() {
            Some(Token::Eof) => {
                break;
            }
            Some(Token::Semicolon) => {
                toks.pop();
                continue;
            }
            Some(Token::Def) => {
                let func = parse_definition(toks)?;
                ast_nodes.push(ASTNode::FunctionNode(*func));
                println!("parsed definition");
            },
            Some(Token::Extern) => {
                let ext = parse_extern(toks)?;
                ast_nodes.push(ASTNode::ExternNode(*ext));
                println!("parsed extern");
            },
            _ => {
                let expr = parse_toplevel_expr(toks)?;
                ast_nodes.push(ASTNode::FunctionNode(*expr));
                println!("parsed toplevel");
            }
        }
    }
    Ok(ast_nodes)
}

fn parse_definition(toks: &mut Vec<Token>) -> Result<Box<Function>> {
    match toks.pop() {
        Some(Token::Def) => (),
        _ => return Err(ParseError("expected 'def'".into())),
    }

    let proto = parse_prototype(toks)?;
    let body = parse_expr(toks)?;
    Ok(Box::new(Function { proto, body }))
}

fn parse_extern(toks: &mut Vec<Token>) -> Result<Box<Prototype>> {
    match toks.pop() {
        Some(Token::Extern) => (),
        _ => return Err(ParseError("expected 'extern'".into())),
    }
    parse_prototype(toks)
}

fn parse_prototype(toks: &mut Vec<Token>) -> Result<Box<Prototype>> {
    let name = match toks.pop() {
        Some(Token::Identifier(id)) => id,
        _ => return Err(ParseError("expected function name".into())),
    };

    match toks.pop() {
        Some(Token::LParen) => (),
        _ => return Err(ParseError("expected '('".into())),
    }

    let mut args: Vec<String> = Vec::new();
    while let Some(Token::Identifier(arg)) = toks.last() {
        args.push(arg.clone());
        toks.pop();
    }

    match toks.pop() {
        Some(Token::RParen) => (),
        _ => return Err(ParseError("expected ')'".into())),
    }

    Ok(Box::new(Prototype { name, args }))
}

fn parse_toplevel_expr(toks: &mut Vec<Token>) -> Result<Box<Function>> {
    let proto = Box::new(Prototype {
        name: "".into(),
        args: Vec::new(),
    });
    let body = parse_expr(toks)?;
    Ok(Box::new(Function { proto, body }))
}

fn parse_expr(toks: &mut Vec<Token>) -> Result<Box<Expr>> {
    let expr = parse_primary_expr(toks)?;
    match toks.last() {
        Some(Token::Operator(_)) => parse_binary_expr_rhs(toks, 0, expr),
        _ => Ok(expr),
    }
}

fn parse_primary_expr(toks: &mut Vec<Token>) -> Result<Box<Expr>> {
    match toks.last() {
        Some(&Token::Identifier(_)) => parse_identifier(toks),
        Some(&Token::Number(_)) => parse_number(toks),
        Some(&Token::LParen) => parse_paren_expr(toks),
        Some(t) => Err(ParseError(format!(
            "expected primary expression, got {:?}",
            *t
        ))),
        None => Err(ParseError(
            "expected primary expression, got nothing".into(),
        )),
    }
}

fn parse_identifier(toks: &mut Vec<Token>) -> Result<Box<Expr>> {
    let id = match toks.pop() {
        Some(Token::Identifier(id)) => id,
        _ => return Err(ParseError("expected identifier".into())),
    };

    // Variable reference
    match toks.last() {
        Some(&Token::LParen) => (),
        _ => return Ok(Box::new(Expr::Variable(id))),
    }

    // Call
    toks.pop(); // LParen
    let mut args: Vec<Box<Expr>> = Vec::new();
    loop {
        args.push(parse_expr(toks)?);
        match toks.last() {
            Some(&Token::RParen) => break,
            _ => (),
        }
        match toks.last() {
            Some(&Token::Comma) => (),
            _ => return Err(ParseError("expected ')' or ','".into())),
        }
        toks.pop(); // Comma
    }
    toks.pop(); // RParen
    Ok(Box::new(Expr::Call { callee: id, args }))
}

fn parse_number(toks: &mut Vec<Token>) -> Result<Box<Expr>> {
    let n = match toks.pop() {
        Some(Token::Number(n)) => n,
        _ => return Err(ParseError("expected number".into())),
    };
    Ok(Box::new(Expr::Number(n)))
}

fn parse_paren_expr(toks: &mut Vec<Token>) -> Result<Box<Expr>> {
    match toks.pop() {
        Some(Token::LParen) => (),
        _ => return Err(ParseError("expected '('".into())),
    }

    let expr = parse_expr(toks);
    match toks.pop() {
        Some(Token::RParen) => (),
        _ => return Err(ParseError("expected ')'".into())),
    }
    expr
}

fn parse_binary_expr_rhs(
    toks: &mut Vec<Token>,
    mut expr_prec: i32,
    mut lhs: Box<Expr>,
) -> Result<Box<Expr>> {
    loop {
        let op = match toks.last() {
            Some(Token::Operator(op)) => {
                let tok_prec = get_tok_precedence(op)?;
                if tok_prec < expr_prec {
                    return Ok(lhs);
                }
                expr_prec = tok_prec;
                op.clone()
            }
            _ => return Ok(lhs),
        };

        toks.pop();
        let mut rhs = parse_primary_expr(toks)?;
        if let Some(Token::Operator(next_op)) = toks.last() {
            let tok_prec = get_tok_precedence(next_op)?;
            if tok_prec > expr_prec {
                rhs = parse_binary_expr_rhs(toks, tok_prec, rhs)?;
            }
        }

        lhs = Box::new(Expr::Binary { op, lhs, rhs });
    }
}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for ParseError {}

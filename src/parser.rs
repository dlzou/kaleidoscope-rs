use std::{collections::HashMap, error::Error, fmt};

use crate::lexer::{LexError, Token, TokenKind, TokenPosition, TokenStream};

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Unary {
        op: String,
        operand: Box<Expr>,
    },
    Binary {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    If {
        cond_expr: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    For {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Box<Expr>,
    },
    Var {
        vars: Vec<(String, Option<Expr>)>,
        body: Box<Expr>,
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
    pub is_op: bool,
    pub precedence: usize,
}

#[derive(Debug)]
pub struct Function {
    pub proto: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

macro_rules! pop_token_expect {
    ( $tokens:expr , $kind:pat => $ret:expr ) => {
        match $tokens.pop()? {
            Token {
                kind: $kind,
                pos: _,
            } => $ret,
            Token { kind: k, pos: p } => {
                return Err(UnexpectedTokenError { kind: k, pos: p }.into())
            }
        }
    };
}

const ANON_FUNC_NAME: &str = "__anon";

const DEFAULT_PRECEDENCE: usize = 30;

pub struct Parser<'a, 'b> {
    tokens: TokenStream<'a>,
    op_prec: &'b HashMap<String, usize>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: TokenStream<'a>, op_prec: &'b HashMap<String, usize>) -> Self {
        Self { tokens, op_prec }
    }

    fn get_op_precedence(&self, op: &String) -> Result<usize> {
        match self.op_prec.get(op) {
            Some(&p) => Ok(p),
            None => Err(ParseError::Other(format!("unknown operator '{op}'"))),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Function>> {
        let mut ast_nodes: Vec<Function> = Vec::new();
        loop {
            match self.tokens.peek_kind() {
                TokenKind::Eof => {
                    break;
                }
                TokenKind::Semicolon => {
                    self.tokens.pop()?;
                    continue;
                }
                TokenKind::Def => {
                    let func = self.parse_definition()?;
                    ast_nodes.push(func);
                }
                TokenKind::Extern => {
                    let ext = self.parse_extern()?;
                    ast_nodes.push(ext);
                }
                _ => {
                    let top = self.parse_toplevel_expr()?;
                    ast_nodes.push(top);
                }
            }
        }
        Ok(ast_nodes)
    }

    fn parse_definition(&mut self) -> Result<Function> {
        pop_token_expect!(self.tokens, TokenKind::Def => ());

        let proto = self.parse_prototype()?;
        let body = self.parse_expr()?;
        Ok(Function {
            proto,
            body: Some(body),
            is_anon: false,
        })
    }

    fn parse_extern(&mut self) -> Result<Function> {
        pop_token_expect!(self.tokens, TokenKind::Extern => ());

        let proto = self.parse_prototype()?;
        Ok(Function {
            proto,
            body: None,
            is_anon: false,
        })
    }

    fn parse_prototype(&mut self) -> Result<Prototype> {
        // let name = pop_token_expect!(tokens : TokenKind::Identifier(id) => id);

        let mut proto_kind = 0; // 0 = identifier, 1 = unary, 2 = binary
        let mut precedence = DEFAULT_PRECEDENCE;

        let name = match self.tokens.pop()? {
            Token {
                kind: TokenKind::Identifier(id),
                pos: _,
            } => id,

            Token {
                kind: TokenKind::Unary,
                pos: _,
            } => {
                proto_kind = 1;
                pop_token_expect!(self.tokens, TokenKind::Operator(op) => op)
            }

            Token {
                kind: TokenKind::Binary,
                pos: _,
            } => {
                proto_kind = 2;
                let name = pop_token_expect!(self.tokens, TokenKind::Operator(op) => op);
                if let &TokenKind::Number(n) = self.tokens.peek_kind() {
                    precedence = n as usize;
                    self.tokens.pop()?;
                }
                name
            }

            Token { kind: k, pos: p } => {
                return Err(UnexpectedTokenError { kind: k, pos: p }.into());
            }
        };

        pop_token_expect!(self.tokens, TokenKind::LParen => ());

        let mut params: Vec<String> = Vec::new();
        loop {
            match self.tokens.peek_kind() {
                TokenKind::RParen => break,
                TokenKind::Identifier(p) => {
                    params.push(p.clone());
                    self.tokens.pop()?;
                }
                k => {
                    return Err(UnexpectedTokenError {
                        kind: k.clone(),
                        pos: self.tokens.peek_pos().clone(),
                    }
                    .into())
                }
            }
            match self.tokens.peek_kind() {
                TokenKind::RParen => break,
                TokenKind::Comma => {
                    self.tokens.pop()?;
                }
                k => {
                    return Err(UnexpectedTokenError {
                        kind: k.clone(),
                        pos: self.tokens.peek_pos().clone(),
                    }
                    .into())
                }
            }
        }

        pop_token_expect!(self.tokens, TokenKind::RParen => ());

        let is_op = proto_kind > 0;
        if is_op && proto_kind != params.len() {
            return Err(ParseError::Other(
                "invalid number of operands for operator".into(),
            ));
        }
        Ok(Prototype {
            name,
            params,
            is_op,
            precedence,
        })
    }

    fn parse_toplevel_expr(&mut self) -> Result<Function> {
        let proto = Prototype {
            name: ANON_FUNC_NAME.into(),
            params: Vec::new(),
            is_op: false,
            precedence: DEFAULT_PRECEDENCE,
        };
        let body = self.parse_expr()?;
        Ok(Function {
            proto,
            body: Some(body),
            is_anon: true,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        let lhs = self.parse_unary_expr()?;
        self.parse_binary_expr_rhs(0, lhs)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        match self.tokens.peek_kind() {
            TokenKind::Operator(op) => {
                let op = op.clone();
                self.tokens.pop()?;
                let unary = Expr::Unary {
                    op,
                    operand: Box::new(self.parse_unary_expr()?),
                };
                Ok(unary)
            }
            _ => self.parse_primary_expr(),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        match self.tokens.peek_kind() {
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Number(_) => self.parse_number(),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::For => self.parse_for_expr(),
            TokenKind::Var => self.parse_var_expr(),
            k => {
                return Err(UnexpectedTokenError {
                    kind: k.clone(),
                    pos: self.tokens.peek_pos().clone(),
                }
                .into())
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr> {
        let id = pop_token_expect!(self.tokens, TokenKind::Identifier(id) => id);

        // Variable reference
        match self.tokens.peek_kind() {
            TokenKind::LParen => (),
            _ => return Ok(Expr::Variable(id)),
        }

        // Call
        self.tokens.pop()?; // LParen
        let mut args: Vec<Expr> = Vec::new();
        loop {
            match self.tokens.peek_kind() {
                TokenKind::RParen => break,
                _ => {
                    args.push(self.parse_expr()?);
                }
            }
            match self.tokens.peek_kind() {
                TokenKind::RParen => break,
                TokenKind::Comma => {
                    self.tokens.pop()?;
                }
                k => {
                    return Err(UnexpectedTokenError {
                        kind: k.clone(),
                        pos: self.tokens.peek_pos().clone(),
                    }
                    .into())
                }
            }
        }
        self.tokens.pop()?; // RParen
        Ok(Expr::Call { callee: id, args })
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let n = pop_token_expect!(self.tokens, TokenKind::Number(n) => n);
        Ok(Expr::Number(n))
    }

    fn parse_paren_expr(&mut self) -> Result<Expr> {
        pop_token_expect!(self.tokens, TokenKind::LParen => ());
        let expr = self.parse_expr();
        pop_token_expect!(self.tokens, TokenKind::RParen => ());
        expr
    }

    fn parse_if_expr(&mut self) -> Result<Expr> {
        pop_token_expect!(self.tokens, TokenKind::If => ());
        let cond_expr = Box::new(self.parse_expr()?);

        pop_token_expect!(self.tokens, TokenKind::Then => ());
        let then_expr = Box::new(self.parse_expr()?);

        pop_token_expect!(self.tokens, TokenKind::Else => ());
        let else_expr = Box::new(self.parse_expr()?);

        Ok(Expr::If {
            cond_expr,
            then_expr,
            else_expr,
        })
    }

    fn parse_for_expr(&mut self) -> Result<Expr> {
        pop_token_expect!(self.tokens, TokenKind::For => ());
        let var = pop_token_expect!(self.tokens, TokenKind::Identifier(id) => id);
        match self.tokens.pop()? {
            Token {
                kind: TokenKind::Operator(op),
                pos: _,
            } if op == "=" => (),
            Token { kind: k, pos: p } => {
                return Err(UnexpectedTokenError { kind: k, pos: p }.into())
            }
        }
        let start = Box::new(self.parse_expr()?);

        pop_token_expect!(self.tokens, TokenKind::Comma => ());
        let end = Box::new(self.parse_expr()?);

        let mut step = None;
        if let TokenKind::Comma = self.tokens.peek_kind() {
            self.tokens.pop()?;
            step = Some(Box::new(self.parse_expr()?));
        }

        pop_token_expect!(self.tokens, TokenKind::In => ());

        let body = Box::new(self.parse_expr()?);

        Ok(Expr::For {
            var,
            start,
            end,
            step,
            body,
        })
    }

    fn parse_var_expr(&mut self) -> Result<Expr> {
        pop_token_expect!(self.tokens, TokenKind::Var => ());

        let mut vars = Vec::new();
        loop {
            let id = pop_token_expect!(self.tokens, TokenKind::Identifier(id) => id);

            match self.tokens.peek_kind() {
                TokenKind::Operator(op) if op == "=" => {
                    self.tokens.pop()?;
                    vars.push((id, Some(self.parse_expr()?)));
                }
                TokenKind::Comma => {
                    self.tokens.pop()?;
                    vars.push((id, None));
                    continue;
                }
                TokenKind::In => {
                    vars.push((id, None));
                    break;
                }
                k => {
                    return Err(UnexpectedTokenError {
                        kind: k.clone(),
                        pos: self.tokens.peek_pos().clone(),
                    }
                    .into())
                }
            }
            match self.tokens.peek_kind() {
                TokenKind::Comma => {
                    self.tokens.pop()?;
                }
                _ => break,
            }
        }

        pop_token_expect!(self.tokens, TokenKind::In => ());
        let body = self.parse_expr()?;
        Ok(Expr::Var {
            vars,
            body: Box::new(body),
        })
    }

    fn parse_binary_expr_rhs(&mut self, expr_prec: usize, mut lhs: Expr) -> Result<Expr> {
        loop {
            let (op, op_prec) = if let TokenKind::Operator(op) = self.tokens.peek_kind() {
                let op_prec = self.get_op_precedence(op)?;
                if op_prec < expr_prec {
                    return Ok(lhs);
                }
                (op.clone(), op_prec)
            } else {
                return Ok(lhs);
            };

            self.tokens.pop()?; // op
            let mut rhs = self.parse_unary_expr()?;
            if let TokenKind::Operator(next_op) = self.tokens.peek_kind() {
                let next_op_prec = self.get_op_precedence(next_op)?;
                if op_prec < next_op_prec {
                    rhs = self.parse_binary_expr_rhs(op_prec + 1, rhs)?;
                }
            }

            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
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
    use std::collections::HashMap;

    use crate::{lexer::TokenStream, parser::Parser};

    #[test]
    fn parse_expr_binary_1() {
        let op_prec = HashMap::from([("+".into(), 20), ("*".into(), 30)]);
        let tokens = TokenStream::new("a + b * c").unwrap();
        let mut parser = Parser::new(tokens, &op_prec);
        let res = parser.parse_expr().unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Variable(\"a\"), rhs: Binary { op: \"*\", lhs: Variable(\"b\"), rhs: Variable(\"c\") } }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_expr_binary_2() {
        let op_prec = HashMap::from([("+".into(), 20), ("*".into(), 30)]);
        let tokens = TokenStream::new("a * b + c").unwrap();
        let mut parser = Parser::new(tokens, &op_prec);
        let res = parser.parse_expr().unwrap();
        assert_eq!(
            "Binary { op: \"+\", lhs: Binary { op: \"*\", lhs: Variable(\"a\"), rhs: Variable(\"b\") }, rhs: Variable(\"c\") }",
            format!("{:?}", res),
        );
    }

    #[test]
    fn parse_identifier_call() {
        let op_prec = HashMap::from([("+".into(), 20), ("*".into(), 30)]);
        let tokens = TokenStream::new("f(1, 2,)").unwrap();
        let mut parser = Parser::new(tokens, &op_prec);
        let res = parser.parse_expr().unwrap();
        assert_eq!(
            "Call { callee: \"f\", args: [Number(1.0), Number(2.0)] }",
            format!("{:?}", res),
        );
    }
}

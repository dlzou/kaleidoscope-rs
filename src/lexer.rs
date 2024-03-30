use std::{error::Error, fmt};

use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Eof,
    Def,
    Extern,
    LParen,
    RParen,
    Comma,
    Semicolon,
    Identifier(String),
    Number(f64),
    Operator(String),
}

#[derive(Copy, Clone)]
pub struct TokenPosition {
    pub row: usize,
    pub col: usize,
}

impl fmt::Debug for TokenPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl fmt::Display for TokenPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: TokenPosition,
}

pub struct Lexer<'a> {
    input: &'a str,
    index: usize,
    curr_pos: TokenPosition,
    re: Regex,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let patterns = concat!(
            r"^[^\S\n]*(",
            r"(?<newline>\n)|",              // Newline
            r"(?<comment>#.*\n)|",           // Comment
            r"(?<identifier>[a-zA-Z_]\w*)|", // Identifier
            r"(?<number>\d+\.?\d*)|",        // Number
            r"(?<lparen>\()|",               // Left parenthesis
            r"(?<rparen>\))|",               // Right parenthesis
            r"(?<comma>,)|",                 // Comma
            r"(?<semicolon>;)|",             // Semicolon
            r"(?<operator>[\+\-\*/<>=]+)|",  // Operator
            r")",
        );
        let re = Regex::new(patterns).unwrap();

        Lexer {
            input,
            index: 0,
            curr_pos: TokenPosition { row: 1, col: 1 },
            re,
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if self.index > self.input.len() {
            return Err(LexError::EndOfFile);
        }

        loop {
            let offset_input = &self.input[self.index..];
            let cap = self.re.captures(offset_input).unwrap();

            self.index += cap.get(0).unwrap().end();
            let last_pos = self.curr_pos;
            self.curr_pos.col += cap.get(0).unwrap().as_str().len();

            if let Some(_) = cap.name("newline") {
                self.curr_pos.row += 1;
                self.curr_pos.col = 1;
                continue;
            } else if let Some(_) = cap.name("comment") {
                self.curr_pos.row += 1;
                self.curr_pos.col = 1;
                continue;
            } else if let Some(m) = cap.name("identifier") {
                match m.as_str() {
                    "def" => {
                        return Ok(Token {
                            kind: TokenKind::Def,
                            pos: last_pos,
                        })
                    }
                    "extern" => {
                        return Ok(Token {
                            kind: TokenKind::Extern,
                            pos: last_pos,
                        })
                    }
                    other => {
                        return Ok(Token {
                            kind: TokenKind::Identifier(other.into()),
                            pos: last_pos,
                        });
                    }
                }
            } else if let Some(m) = cap.name("number") {
                return Ok(Token {
                    kind: TokenKind::Number(m.as_str().parse().unwrap()),
                    pos: last_pos,
                });
            } else if let Some(_) = cap.name("lparen") {
                return Ok(Token {
                    kind: TokenKind::LParen,
                    pos: last_pos,
                });
            } else if let Some(_) = cap.name("rparen") {
                return Ok(Token {
                    kind: TokenKind::RParen,
                    pos: last_pos,
                });
            } else if let Some(_) = cap.name("comma") {
                return Ok(Token {
                    kind: TokenKind::Comma,
                    pos: last_pos,
                });
            } else if let Some(_) = cap.name("semicolon") {
                return Ok(Token {
                    kind: TokenKind::Semicolon,
                    pos: last_pos,
                });
            } else if let Some(m) = cap.name("operator") {
                return Ok(Token {
                    kind: TokenKind::Operator(m.as_str().into()),
                    pos: last_pos,
                });
            } else if self.index == self.input.len() {
                self.index += 1;
                return Ok(Token {
                    kind: TokenKind::Eof,
                    pos: last_pos,
                });
            }
            return Err(LexError::InvalidCharSeq(last_pos));
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(t) => Some(t),
            Err(_) => None,
        }
    }
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    next_tok: Option<Token>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Result<Self> {
        let mut lexer = Lexer::new(input);
        let curr_tok = lexer.next_token()?;
        Ok(TokenStream {
            lexer,
            next_tok: Some(curr_tok),
        })
    }
    
    pub fn peek_kind(&self) -> &TokenKind {
        &self.next_tok.as_ref().unwrap().kind
    }

    pub fn peek_pos(&self) -> &TokenPosition {
        &self.next_tok.as_ref().unwrap().pos
    }
    
    pub fn pop(&mut self) -> Result<Token> {
        let tok = self.next_tok.take();
        self.next_tok = Some(self.lexer.next_token()?);
        Ok(tok.unwrap())
    }
}

type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug)]
pub enum LexError {
    EndOfFile,
    InvalidCharSeq(TokenPosition)
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::EndOfFile => {
                write!(f, "reached end of file")
            }
            Self::InvalidCharSeq(pos) => {
                write!(f, "invalid character sequence starting from {}", pos)
            }
        }
    }
}

impl Error for LexError {}

use std::{error::Error, fmt};

use regex::Regex;

#[derive(Debug)]
pub enum Token {
    Def,
    Extern,
    LPar,
    RPar,
    Identifier(String),
    Number(f64),
    Other(u8),
}

pub struct Tokenizer<'a> {
    input: &'a str,
    index: usize,
    re: Regex,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        let patterns = concat!(
            r"^\s*(",
            r"(?<comment>#.*\n)|",           // Comment
            r"(?<identifier>[a-zA-Z_]\w*)|", // Identifier
            r"(?<number>\d+\.?\d*)|",        // Number
            r"(?<lpar>\()|",                 // Left parenthesis
            r"(?<rpar>\))",                  // Right parenthesis
            r")",
        );
        let re = Regex::new(patterns).unwrap();

        Tokenizer {
            input,
            index: 0,
            re,
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        if self.index >= self.input.len() {
            return Err(LexError("index larger than input length".into()));
        }

        loop {
            let offset_input = &self.input[self.index..];
            let cap = match self.re.captures(offset_input) {
                Some(c) => {
                    // println!("{:?}", c);
                    c
                }
                None => {
                    return Err(LexError(format!(
                        "bad token at index={} starting with '{}'",
                        self.index,
                        offset_input.chars().next().unwrap(),
                    )));
                }
            };
            self.index += cap.get(0).unwrap().end();

            if let Some(_) = cap.name("comment") {
                continue;
            } else if let Some(m) = cap.name("identifier") {
                match m.as_str() {
                    "def" => {
                        return Ok(Token::Def);
                    }
                    "extern" => {
                        return Ok(Token::Extern);
                    }
                    other => {
                        return Ok(Token::Identifier(other.into()));
                    }
                }
            } else if let Some(m) = cap.name("number") {
                match m.as_str().parse() {
                    Ok(n) => return Ok(Token::Number(n)),
                    Err(_) => return Err(LexError("failed to parse number".into())),
                }
            } else if let Some(_) = cap.name("lpar") {
                return Ok(Token::LPar);
            } else if let Some(_) = cap.name("rpar") {
                return Ok(Token::RPar);
            } else {
                return Err(LexError(format!("wtf")));
            }
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.input.len() {
            return None;
        } else {
            return Some(self.next_token());
        }
    }
}

type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug)]
pub struct LexError(String);

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for LexError {}

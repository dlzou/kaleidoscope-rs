use std::{
    fs,
    io::{self, Write},
};

use kaleidoscope_rs::{lexer::Lexer, parser::parse};

fn test_lexer() {
    let input = fs::read_to_string("examples/tokens.ks").unwrap();

    let mut lexer = Lexer::new(&input);

    while let Some(res) = lexer.next() {
        match res {
            Ok(tok) => println!("{:?}", tok),
            Err(e) => {
                println!("{e}");
                return;
            }
        }
    }
}

fn main() {
    loop {
        print!(">>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        let bytes = io::stdin().read_line(&mut input).unwrap();
        if bytes == 0 {
            return;
        }

        let lexer = Lexer::new(&input);
        let mut tokens: Vec<_> = lexer.map(|tok| tok.unwrap()).collect();
        tokens.reverse();
        let res = parse(&mut tokens);
        println!("{:?}", res);
    }
}

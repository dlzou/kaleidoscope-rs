use std::{
    fs,
    io::{self, Write},
};

use inkwell::{context::Context, values::AnyValue};
use kaleidoscope_rs::{
    compiler::compile,
    lexer::{Lexer, TokenStream},
    parser::parse,
};

fn test_lexer() {
    let input = fs::read_to_string("test_samples/tokens.ks").unwrap();

    let mut lexer = Lexer::new(&input);

    while let Ok(tok) = lexer.next_token() {
        println!("{:?}", tok);
    }

    loop {
        match lexer.next_token() {
            Ok(tok) => {
                println!("{:?}", tok);
            }
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
}

fn test_parser() {
    loop {
        print!(">>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        let bytes = io::stdin().read_line(&mut input).unwrap();
        if bytes == 0 {
            return;
        }

        let lexer = Lexer::new(&input);
        let mut tokens: Vec<_> = lexer.map(|token| token.kind).collect();
        tokens.reverse();
        let res = parse(&mut tokens);
        println!("{:?}", res);
    }
}

fn test_codegen() {
    let context = Context::create();
    let builder = context.create_builder();
    loop {
        print!(">>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        let bytes = io::stdin().read_line(&mut input).unwrap();
        if bytes == 0 {
            return;
        }

        let lexer = Lexer::new(&input);
        let mut tokens: Vec<_> = lexer.map(|token| token.kind).collect();
        tokens.reverse();
        let parsed = parse(&mut tokens);
        println!("{:?}", parsed);
        if parsed.is_err() {
            continue;
        }

        let module = context.create_module("tmp");
        for func in parsed.unwrap() {
            if let Ok(compiled) = compile(&context, &builder, &module, &func) {
                compiled.print_to_stderr();
            };
        }
    }
}

fn main() {
    test_codegen();
}

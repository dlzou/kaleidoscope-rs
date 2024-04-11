use std::{
    fs,
    io::{self, Write},
};

use inkwell::context::Context;
use kaleidoscope_rs::{
    compiler::{compile, run_passes},
    lexer::{Lexer, TokenStream},
    parser::parse,
};

fn run_lexer() {
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

fn run_parser() {
    loop {
        print!("\n>>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        let bytes = io::stdin().read_line(&mut input).unwrap();
        if bytes == 0 {
            return;
        }

        let mut tokens = TokenStream::new(&input).unwrap();
        let parsed = parse(&mut tokens);
        println!("{:?}", parsed);
    }
}

fn run_compiler() {
    let mut display_steps = false;
    for arg in std::env::args() {
        match arg.as_str() {
            "--steps" => display_steps = true,
            _ => (),
        }
    }

    let context = Context::create();
    let builder = context.create_builder();

    let mut prev_funcs = Vec::new();

    loop {
        print!("\n>>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        let bytes = io::stdin().read_line(&mut input).unwrap();
        if bytes == 0 {
            return;
        }

        let mut tokens = TokenStream::new(&input).unwrap();
        let parsed = parse(&mut tokens);
        if parsed.is_err() {
            println!("!!! {:?}", parsed);
            continue;
        }
        if display_steps {
            println!("\nParser output:");
            println!("{:?}", parsed);
        }

        for func in parsed.unwrap() {
            let module = context.create_module("tmp");
            for p in &prev_funcs {
                compile(&context, &builder, &module, p).unwrap();
            }
            let fv = match compile(&context, &builder, &module, &func) {
                Ok(fv) => {
                    if display_steps {
                        println!("\nLLVM unoptimized output:");
                        fv.print_to_stderr();
                    }
                    if !func.is_anon {
                        prev_funcs.push(func);
                        continue;
                    }
                    fv
                }
                Err(e) => {
                    println!("!!! {:?}", e);
                    continue;
                }
            };

            // assert!(func.is_anon);
            run_passes(&module);

            let engine = module
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .unwrap();
            let f_jit = unsafe {
                engine
                    .get_function::<unsafe extern "C" fn() -> f64>(fv.get_name().to_str().unwrap())
            };

            match f_jit {
                Ok(f) => unsafe {
                    println!("{}", f.call());
                },
                Err(e) => {
                    println!("!!! {:?}", e);
                }
            }
        }
    }
}

fn main() {
    run_compiler();
}

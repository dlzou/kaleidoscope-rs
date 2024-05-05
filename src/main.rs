use std::{
    collections::HashMap,
    io::{self, Write},
};

use inkwell::context::Context;
use kaleidoscope_rs::{
    compiler::{compile, run_passes},
    lexer::TokenStream,
    parser::Parser,
};

fn run_compiler() {
    let mut display_steps = false;
    for arg in std::env::args() {
        match arg.as_str() {
            "--steps" => display_steps = true,
            _ => (),
        }
    }

    let mut op_prec = HashMap::from([
        ("=".into(), 2),
        ("<".into(), 10),
        (">".into(), 10),
        ("+".into(), 20),
        ("-".into(), 20),
        ("*".into(), 30),
        ("/".into(), 30),
    ]);

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

        let tokens = TokenStream::new(&input).unwrap();
        let mut parser = Parser::new(tokens, &op_prec);
        let parsed = parser.parse();
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
                compile(&context, &builder, &module, p, &mut op_prec).unwrap();
            }
            let fv = match compile(&context, &builder, &module, &func, &mut op_prec) {
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

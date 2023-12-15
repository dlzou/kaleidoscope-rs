use std::fs;

use kaleidoscope_rs::lexer::Tokenizer;

fn main() {
    let input = fs::read_to_string("examples/tokens.ks").unwrap();

    let mut tokenizer = Tokenizer::new(&input);

    while let Some(res) = tokenizer.next() {
        match res {
            Ok(tok) => println!("{:?}", tok),
            Err(e) => {
                println!("{e}");
                return;
            }
        }
    }
}

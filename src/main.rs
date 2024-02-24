use std::fs;

mod lexer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string("test.tp")?;

    let tokens = lexer::Lexer::new(&source).lex();
    
    assert!(tokens.errors.is_empty());

    for tok in tokens.tokens {
        println!("{:?}", tok.data);
    }

    Ok(())
}

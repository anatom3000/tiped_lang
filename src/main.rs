use std::fs;

mod lexer;
mod parser;
mod tree;
mod typer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Some(source_file_path) = std::env::args().nth(1)
        else { return Err("a source file was expected but no argument was provided".into()) };

    let source = fs::read_to_string(source_file_path)?;
    let tokens = lexer::Lexer::new(&source).lex();

    assert!(tokens.errors.is_empty());

    let mut expr = parser::Parser::new(tokens.tokens).parse();
    typer::infer_type(&mut expr);
    
    println!("Type of program: {}", expr.type_.expect("expr was typed above"));

    Ok(())
}

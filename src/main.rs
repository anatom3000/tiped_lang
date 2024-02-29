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

    let expr = parser::Parser::new(tokens.tokens).parse();

    println!("{expr}");
    
    use crate::tree::Type::*;

    macro_rules! t {
        (v $x:expr) => { Box::new(Var($x.into())) };
        (a $x:expr) => { Box::new(Atom($x.into())) };
        (f $from:expr => $to:expr) => { Box::new(Fun($from, $to)) }
    }

    let e1 = Fun(
        t!(v "x"),
        t!(v "y")
    );

    let e2 = Fun(
        t!(a "a"),
        t!(f t!(a "a") => t!(a "b"))
    );

    println!("{}", typer::unify(e1, e2));

    Ok(())
}
